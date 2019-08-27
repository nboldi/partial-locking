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
           
module Demo3.ManualLocking where

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
import qualified Data.Vector as Vec
import qualified Data.ByteString.Lazy as BS
import System.TimeIt

data Time
  = Time
    { _year    :: Int
    , _month   :: Int
    , _day     :: Int
    , _hour    :: Int
    , _minute  :: Int
    , _sec     :: Int
    }
  
instance Show Time where
  show (Time year mon day hour min sec)
    = show year ++ "/" ++ show mon ++ "/" ++ show day
        ++ " " ++ show hour ++ ":" ++ show min ++ ":" ++ show sec    
  
data Log
  = Log { _loggerThread     :: String
        , _msg              :: String
        , _loggingDate      :: MVar Time
        } 

data LogList = LogList { _logListData :: Fair (ConcurrentList Fair (MVar Log))
                       , _logListNum :: MVar Int
                       }
    
data ConcurrentList lc a = CLNil | CLCons a (lc (ConcurrentList lc a))
	
clAppend :: Log -> LogList -> IO LogList
clAppend log ll
  = do mv <- newMVar log
       modifyMVar_ (_logListNum ll) (return . (+1))
       newLL <- newFair $ CLCons mv (_logListData ll)
       return $ ll { _logListData = newLL } 
    
clIndex :: Int -> Fair (ConcurrentList Fair (MVar Log)) -> IO Log
clIndex i ls 
  = do readMVar (_fairGrReadEntry ls) 
       modifyMVar_ (_fairGrCounter ls) 
                   (\cnt -> do when (cnt == 0) (takeMVar (_fairGrWrite ls))
                               return (cnt+1))
       lsData <- readIORef (_fairData ls)
       res <- case lsData of 
                CLNil -> error "clIndex: index too large"
                CLCons a rest -> if i == 0 then readMVar a else clIndex (i-1) rest
       modifyMVar_ (_fairGrCounter ls) 
                   (\cnt -> do when (cnt == 1) $ putMVar (_fairGrWrite ls) ()
                               return (cnt-1))
       return res
    
data LogDB
  = LogDB { _criticalErrors :: LogList
          , _errors         :: LogList
          , _debugInfos     :: LogList
          , _startDate      :: MVar Time
          , _lastLogDate    :: MVar Time
          }
          
data Fair a
  = Fair { _fairGrCounter :: MVar Int -- ^ number of readers currently working on the data
         , _fairGrReadEntry :: MVar () -- ^ limit, when readers can enter to stop readers from starving out writers
		 , _fairGrWrite :: MVar () -- ^ write lock, could be held by one writer or a set of readers
         , _fairData :: IORef a
         }
		 
newFair :: a -> IO (Fair a)
newFair a = Fair <$> newMVar 0 <*> newMVar () <*> newMVar () <*> newIORef a

 
data LogDBWrapper = LogDBWrapper { _wrappedDB :: IORef LogDB }
         
emptyLogDB :: IO LogDB
emptyLogDB = do logList1 <- LogList <$> newFair CLNil <*> newMVar 0
                logList2 <- LogList <$> newFair CLNil <*> newMVar 0
                logList3 <- LogList <$> newFair CLNil <*> newMVar 0
                time1 <- getTime >>= newMVar 
                time2 <- getTime >>= newMVar
                return $ LogDB logList1 logList2 logList3 time1 time2
         
getTime :: IO Time
getTime = do (y,mo,d,h,mi,s) <- toGregorian <$> getCurrentTime
             return $ Time (fromIntegral y) mo d h mi s
        
data Config = Config { readRatio :: Double
                     , lengthSecs :: Int
                     }
type Message = (String, String)

--------------------------

data LoggerMsg = LogThat Log
               | LogQuery (MVar (Maybe String))
      
categoryMeasure repeatNum 
  = do resf <- openFile "../manual-locking-measure-cats.csv" WriteMode
       BS.hPutStr resf $ encode [["Read-Write ratio", "Average performance"]]
       results <- mapM (\(r,cfg) -> (r,) . normalizeTest cfg <$> runFor cfg repeatNum []) measureConfs
       
       BS.hPutStr resf $ encode results
       hClose resf
  where measureConfs :: [(Double, Config)]
        measureConfs 
          = map (\r -> (r, mainCfg {readRatio = r})) [0.0,0.1..1.0]
                                                   
average xs = realToFrac (sum xs) / genericLength xs
normalizeTest :: Config -> [(Double,Double)] -> Double
normalizeTest conf = average . map (\(a,b) -> a + b)
 
mainCfg = Config { readRatio = 0.75
                 , lengthSecs = 2
                 }
       
mainRunFor = runFor mainCfg 5 [] >>= print
       
runFor :: Config -> Int -> [(Double,Double)] -> IO [(Double,Double)]
runFor cfg numResults results 
  = do numLogs <- newMVar (0,0)
       (time, res) <- timeItT $ do forkIO (run cfg numLogs 50) >>= timer
                                   tryTakeMVar numLogs
       let results' = maybe id (\p ls -> (both *- ((/time) . fromIntegral) $ p):ls) res $ results
       if (length results' < numResults)
         then runFor cfg numResults results' 
         else return results'
  where timer th = threadDelay (lengthSecs cfg * 1000000) 
                     >> killThread th

mainMeasure = measure 10
      
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
  = do logDB <- emptyLogDB >>= newIORef
       updaters <- newMVar []
       (forever $ 
         takeMVar logChan >>= \q -> 
           forkIO (void $ case q of
               LogThat log -> 
                 (do time <- getTime
                     db <- readIORef logDB
                     modifyMVar_ (_lastLogDate db) (const (return time))
                     ls <- clAppend log (_debugInfos db)
                     writeIORef logDB $ db { _debugInfos = ls }
                     mvar & _1 !- (+1) $ numCheck)
                   
               LogQuery ch -> 
                 do db <- readIORef logDB
                    n <- readMVar (_logListNum (_debugInfos db))              
                    if n > 0 then do
                      i <- evalRandIO (fromList (map (,1) [0..n-1]))
                      elem <- clIndex i (_logListData (_debugInfos db))  
                      putMVar ch (Just (_msg elem))
                      else putMVar ch Nothing                                  
                    mvar & _2 !- (+1) $ numCheck
             ) >>= \fork -> mvar !- (fork:) $ updaters 
         ) `catch` (\e -> terminateThreads updaters e >> checkNum logDB e)
  where terminateThreads updaters ThreadKilled =    
          void $ mvar&traverse *!| killThread $ updaters
        terminateThreads _ e = throw e
        
        checkNum logDB ThreadKilled =
          do db <- readIORef logDB
             logsStored <- readMVar (_logListNum (_debugInfos db))        
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
                     (askLog askChan , toRational $ readRatio cfg),
                     (writeLog thrId , toRational $ 1.0 - readRatio cfg)
                   ])
  where askLog askChan 
          = void $ putMVar logChan (LogQuery askChan) >> takeMVar askChan
        writeLog thrId = do time <- getTime >>= newMVar
                            let log = Log (show thrId) "yollo" time
                            putMVar logChan $ LogThat log





