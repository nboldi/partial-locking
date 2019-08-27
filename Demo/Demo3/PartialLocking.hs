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
           
module Demo3.PartialLocking where

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

import Demo3.Distributed
import Demo3.DerivedRepresentation
import Demo3.TH


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
          
distrLogElem :: Int -> Simple IOPartial' (Distributed [Log]) (Distributed Log)
distrLogElem 0 = alt2_2 & _1 & _lock
distrLogElem i = alt2_2 & _2 & _fair & distrLogElem (i-1)

alt2_1 :: Simple Partial (Alt2 a b) a
alt2_1 = simplePartial (\case (Alt2_1 x) -> Just (x, \x' -> Alt2_1 x'); Alt2_2 _ -> Nothing)
alt2_2 :: Simple Partial (Alt2 a b) b
alt2_2 = simplePartial (\case Alt2_2 x -> Just (x, \x' -> Alt2_2 x'); Alt2_1 _ -> Nothing)

type instance Node LogDB = Fair
type instance Node LogList = Fair
type instance Node [Log] = Fair
type instance Node Log = Lock
type instance Node Time = Lock
type instance Node Size = Lock
type instance Node String = Clean
type instance Node Int = Clean
type instance Node Char = Clean
         
$(makeDerivedRefs ''Time)
$(makeDerivedRefs ''Log)
$(makeDerivedRefs ''LogList)
$(makeDerivedRefs ''Size)
$(makeDerivedRefs ''LogDB)
 
$(makeDistrCons [t| [Log] |])
$(makeDistrCons ''Log)
$(makeDistrCons ''LogList)
$(makeDistrCons ''LogDB)
$(makeDistrCons ''Time)
 
data LogDBWrapper = LogDBWrapper { _wrappedDB :: LogDB } deriving Generic

$(makeDistrCons [t| LogDBWrapper |])
$(makeDerivedRefs ''LogDBWrapper)
         
emptyLogDB :: IO (Distributed LogDB)
emptyLogDB = do logList <- distribute $ LogList [] (Size 0)
                time <- getTime
                _LogDB logList logList logList time time
         
getTime :: IO (Distributed Time)
getTime = do (y,mo,d,h,mi,s) <- toGregorian <$> getCurrentTime
             _Time (fromIntegral y) mo d h mi s
        
data Config = Config { readRatio :: Double
                     , lengthSecs :: Int
                     }
type Message = (String, String)

--------------
       
undistrWrappedLogDB :: Distributed LogDBWrapper -> IO LogDBWrapper
undistrWrappedLogDB (Mem1 db) 
  = LogDBWrapper <$> ((db ^! breakFair) >>= undistrLogDB)
       
undistrLogDB :: Distributed LogDB -> IO LogDB
undistrLogDB (Mem5 a b c d e) 
  = LogDB <$> ((a ^! breakFair) >>= undistrLogList)
          <*> ((b ^! breakFair) >>= undistrLogList)
          <*> ((c ^! breakFair) >>= undistrLogList)
          <*> ((d ^! _lock) >>= undistrTime)
          <*> ((e ^! _lock) >>= undistrTime)

undistrLogList :: Distributed LogList -> IO LogList
undistrLogList (Mem2 d n)
  = LogList <$> ((d ^! breakFair) >>= undistrLogs)    
            <*> ((n ^! _lock) >>= undistrSize) 
          
undistrLogs :: Distributed [Log] -> IO [Log]
undistrLogs (Alt2_1 Mem0) = return []
undistrLogs (Alt2_2 (Mem2 head tail)) 
  = (:) <$> ((head ^! _lock) >>= undistrLog)
        <*> ((tail ^! breakFair) >>= undistrLogs)

undistrLog :: Distributed Log -> IO Log
undistrLog (Mem3 th msg tm) 
  = Log (th ^. _clean) (msg ^. _clean) <$> ((tm ^! _lock) >>= undistrTime)

undistrSize :: Distributed Size -> IO Size
undistrSize (Mem1 (Clean i)) = return $ Size i
  
undistrTime :: Distributed Time -> IO Time
undistrTime (Mem6 year mon day hour min sec) 
  = return $ Time (year ^. _clean) (mon ^. _clean)
                  (day ^. _clean) (hour ^. _clean)
                  (min ^. _clean) (sec ^. _clean)

------------------------
                  
waiter :: Int -> Simple IOLens a a
waiter msecs 
  = let delay :: (MMorph IO m) => m ()
        delay = morph (threadDelay msecs)
     in reference (\s -> delay >> return s)
                  (\b _ -> delay >> return b)
                  (\trf s -> delay >> trf s)

marker :: (String,String) -> Simple IOLens a a
marker (enter,leave)
  = referenceWithClose 
      (\s -> morph (putStr enter) >> return s) (const $ morph (putStr leave))
      (\b _ -> morph (putStr enter) >> return b) (const $ morph (putStr leave))
      (\trf s -> morph (putStr enter) >> trf s) (const $ morph (putStr leave))

--------------------------

data LoggerMsg = LogThat (Distributed Log)
               | LogQuery (MVar (Maybe String))
      

amortizeMeasure repeatNum
  = do resf <- openFile "../partial-locking-measure-amortize.csv" WriteMode
       BS.hPutStr resf $ encode [["Read-Write ratio"] ++ map ((++" s") . show . fst) (snd $ head measureConfs)]
       results <- mapM (\(r,cfgs) -> (r,) <$> 
                    mapM (\(_,cfg) -> normalizeTest cfg <$>
                             runFor cfg repeatNum []) cfgs
                       ) measureConfs
       
       BS.hPutStr resf $ encode 
         $ map (\row -> Vec.cons (fst row) $ Vec.fromList $ snd row) results
       hClose resf
  where measureConfs :: [(Double, [(Int, Config)])]
        measureConfs 
          = map (\r -> (r, map (\s -> (s, mainCfg {readRatio = r, lengthSecs = s})) 
                              [1, 2, 4, 8, 16])) [0.3, 0.5, 0.7]
        
categoryMeasure repeatNum 
  = do resf <- openFile "../partial-locking-measure-cats.csv" WriteMode
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
  = do logDB <- emptyLogDB >>= _LogDBWrapper >>= newIORef
       updaters <- newMVar []
       (forever $ 
         takeMVar logChan >>= \q -> 
           forkIO (void $ case q of
               LogThat log -> 
                 (do time <- getTime
                     (ioref & wrappedDB & lastLogDate != time) 
                       >=> (ioref & wrappedDB & debugInfos & logListData !~ (log %:))
                       >=> (ioref & wrappedDB & debugInfos & logListNum & sizeInt !- (+1))
                       $ logDB
                     mvar & _1 !- (+1) $ numCheck)
                   
               LogQuery ch -> do n <- logDB ^! ioref & wrappedDB & debugInfos & logListNum & sizeInt
                                 if n > 0 then do
                                   i <- evalRandIO (fromList (map (,1) [0..n-1]))
                                   logDB ^?! ioref & wrappedDB & debugInfos 
                                                   & logListData & distrLogElem (fromIntegral i) & msg
                                       >>= putMVar ch   
                                   else putMVar ch Nothing                                  
                                 mvar & _2 !- (+1) $ numCheck
             ) >>= \fork -> mvar !- (fork:) $ updaters 
         ) `catch` (\e -> terminateThreads updaters e >> checkNum logDB e)
  where terminateThreads updaters ThreadKilled =    
          void $ mvar&traverse *!| killThread $ updaters
        terminateThreads _ e = throw e
        
        checkNum logDB ThreadKilled =
          do logsStored <- logDB ^! ioref & wrappedDB & debugInfos & logListNum & sizeInt
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
        writeLog thrId = void $ getTime >>= _Log (show thrId) "yollo"
                                        >>= putMVar logChan . LogThat





