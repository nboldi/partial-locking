{-# OPTIONS_GHC -fcontext-stack=64 #-}
{-# LANGUAGE DefaultSignatures
           , DeriveGeneric
           , LambdaCase
           , TypeOperators
           , FlexibleContexts
           , FlexibleInstances
           , TypeFamilies
           , UndecidableInstances
           , OverlappingInstances
           , MultiParamTypeClasses
           , FunctionalDependencies
           , RankNTypes
           , TemplateHaskell
           , TupleSections
           , StandaloneDeriving
           , ScopedTypeVariables
           , AllowAmbiguousTypes
           #-}
           
module Demo3.CentralLocking where

import GHC.Generics
import Control.Concurrent
import Control.Applicative
import Control.Monad
import Data.DateTime
import Control.Monad.Random
import qualified Control.Monad.Trans.List as Trans
import System.Random
import Data.List
import Data.IORef
import Control.Reference
import Control.Reference.TH.Records (debugTH)
import Control.Exception
import Language.Haskell.TH
import System.IO
import Data.Csv as CSV
import qualified Data.ByteString.Lazy as BS


data Time
  = Time
    { _year    :: Int
    , _month   :: Int
    , _day     :: Int
    , _hour    :: Int
    , _minute  :: Int
    , _sec     :: Int
    }
  deriving (Eq, Generic)
  
instance Show Time where
  show (Time year mon day hour min sec)
    = show year ++ "/" ++ show mon ++ "/" ++ show day
        ++ " " ++ show hour ++ ":" ++ show min ++ ":" ++ show sec    
  
data Log
  = Log { _loggerThread     :: String
        , _msg              :: String
        , _loggingDate      :: Time
        } deriving (Eq, Show, Generic)

data LogList = LogList { _logListData :: [Log]
                       , _logListNum :: Size 
                       } deriving (Eq, Show, Generic)
  
data Size = Size { _sizeInt :: Int } deriving (Eq, Show, Generic)
  
data LogDB
  = LogDB { _criticalErrors :: LogList
          , _errors         :: LogList
          , _debugInfos     :: LogList
          , _startDate      :: Time
          , _lastLogDate    :: Time
          } deriving (Eq, Show, Generic)

$(makeReferences ''Time)
$(makeReferences ''Log)
$(makeReferences ''Size)
$(makeReferences ''LogList)
$(makeReferences ''LogDB)
 
data LogDBWrapper = LogDBWrapper { _wrappedDB :: LogDB } deriving Generic

$(makeReferences ''LogDBWrapper)
         
logAppend :: Log -> [Log] -> IO [Log]
logAppend log ls = return $ log : ls
         
emptyLogDB :: IO LogDB
emptyLogDB = do time <- getTime
                let emptyList = LogList [] (Size 0)
                return $ LogDB emptyList emptyList emptyList time time
         
getTime :: IO Time
getTime = do (y,mo,d,h,mi,s) <- toGregorian <$> getCurrentTime
             return $ Time (fromIntegral y) mo d h mi s
        
data Config = Config { readRatio :: Rational
                     , lengthSecs :: Int
                     }
type Message = (String, String)

data LoggerMsg = LogThat Log
               | LogQuery (MVar (Maybe String))
      
categoryMeasure repeatNum 
  = do resf <- openFile "../central-locking-measure-cats.csv" WriteMode
       BS.hPutStr resf $ encode [["Read-Write ratio", "Average performance"]]
       results <- mapM (\(r,cfg) -> (r,) . normalizeTest cfg <$> runFor cfg repeatNum []) measureConfs
       BS.hPutStr resf $ encode results
       hClose resf       
  where measureConfs :: [(Rational, Config)]
        measureConfs 
          = map (\r -> (r, mainCfg {readRatio = r})) [0.0,0.1..1.0]
                   
average xs = realToFrac (sum xs) / genericLength xs
normalizeTest :: Config -> [(Int,Int)] -> Rational
normalizeTest conf = average . map (\(a,b) -> a + b)
                         
      
instance ToField Rational where
  toField = (toField :: Float -> Field) . fromRational
      
mainCfg = Config { readRatio = 0.75
                 , lengthSecs = 2
                 }
      
runFor :: Config -> Int -> [(Int,Int)] -> IO [(Int,Int)]
runFor cfg numResults results 
  = do numLogs <- newMVar (0,0)
       forkIO (run cfg numLogs 50) >>= timer
       num <- tryTakeMVar numLogs
       let results' = maybe id (:) num results
       if (length results' < numResults)
         then runFor cfg numResults results' 
         else return results'
  where timer th = threadDelay (lengthSecs cfg * 1000000) 
                     >> killThread th

mainMeasure = measure 60
      
measure :: Int -> IO ()
measure secs 
  = do logNum <- newMVar (0,0)
       drawUp >> forkIO (run mainCfg logNum 50)
              >>= timer maxTics
       readMVar logNum >>= (\(writes,reads) -> putStrLn $ show writes ++ " logs written " ++ show reads ++ " queries performed")
  where drawUp = putStrLn (replicate maxTics '=')
        maxTics = 60
        secsPerTic = fromIntegral secs / fromIntegral maxTics
        timer i th = if i > 0 then do
                       threadDelay (round (1000000.0 * secsPerTic))
                       putStr "-"
                       timer (i-1) th
                      else putStrLn "" >> killThread th >> threadDelay 500000

run :: Config -> MVar (Int,Int) -> Int -> IO ()    
run cfg logNum n 
  = do logChan <- newEmptyMVar
       logThread <- forkIO $ logger cfg logNum logChan
       chans <- replicateM n newEmptyMVar
       threads <- (forkIO . worker cfg logChan chans) `mapM` chans
       forever (threadDelay 1000000)
         `catch` \ThreadKilled -> do
                     mapM_ killThread threads
                     killThread logThread

logger :: Config -> MVar (Int,Int) -> MVar LoggerMsg -> IO ()
logger cfg numCheck logChan 
  = do logDB <- emptyLogDB >>= newMVar . LogDBWrapper
       updaters <- newMVar []
       (forever $ 
         takeMVar logChan >>= \q -> 
           forkIO (void $ case q of
               LogThat log -> 
                  do time <- getTime
                     (mvar & wrappedDB & lastLogDate != time) 
                       >=> (mvar & wrappedDB & debugInfos & logListData !~ (logAppend log))
                       >=> (mvar & wrappedDB & debugInfos & logListNum & sizeInt !- (+1))
                       $ logDB
                     mvar & _1 !- (+1) $ numCheck
                   
               LogQuery ch -> do n <- logDB ^! mvar & wrappedDB & debugInfos & logListNum & sizeInt
                                 if n > 0 then do
                                   i <- evalRandIO (fromList (map (,1) [0..n-1]))
                                   logDB ^?! mvar & wrappedDB & debugInfos 
                                                  & logListData & element i & msg
                                       >>= putMVar ch     
                                  else putMVar ch Nothing       
                                 mvar & _2 !- (+1) $ numCheck
             ) >>= \fork -> mvar !- (fork:) $ updaters 
         ) `catch` (\e -> terminateThreads updaters e >> checkNum logDB e)
  where terminateThreads updaters ThreadKilled =    
          void $ mvar&traverse *!| killThread $ updaters
        terminateThreads _ e = throw e
        
        checkNum logDB ThreadKilled =
          do logsStored <- logDB ^! mvar & wrappedDB & debugInfos & logListNum & sizeInt
             check <- numCheck ^! mvar & _1
             when (logsStored < check)
               $ putStrLn ("Missing logs: expected " ++ show check ++ ", but got: " ++ show logsStored)
        checkNum _ e = throw e        

worker :: Config -> MVar LoggerMsg -> [MVar Message] -> MVar Message -> IO ()
worker cfg logChan chans myChan 
  = do askChan <- newEmptyMVar 
       forever $
         do thrId <- myThreadId
            partner <- evalRandIO (fromList (map (,1) chans))
            join $ evalRandIO (fromList [
                     (askLog askChan , readRatio cfg),
                     (writeLog thrId , 1.0 - readRatio cfg)
                   ])
  where askLog askChan 
          = void $ do putMVar logChan (LogQuery askChan)
                      takeMVar askChan
        writeLog thrId = void $ do time <- getTime 
                                   putMVar logChan $ LogThat 
                                     $ Log (show thrId) "yollo" time

