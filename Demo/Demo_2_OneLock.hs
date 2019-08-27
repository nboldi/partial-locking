{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module Demo.OneLock where

import Control.Concurrent
import System.Random
import Control.Monad.Random
import Control.Monad.State
import Control.Monad
import Data.DateTime
import GHC.Generics
import Control.Lens
import Control.Applicative

data Time
  = Time
    { _year    :: Int
    , _month   :: Int
    , _day     :: Int
    , _hour    :: Int
    , _minute  :: Int
    , _sec     :: Int
    }
  deriving (Eq, Show, Generic)
$( makeLenses ''Time )
  
data Log
  = Log { _loggerThread     :: ThreadId
        , _msg              :: String
        , _loggingDate      :: Time
        } deriving Show
$( makeLenses ''Log )

data LogDB
  = LogDB { _criticalErrors :: [Log]
          , _errors         :: [Log]
          , _debugInfos     :: [Log]
          , _startDate      :: Time
          , _lastLogDate    :: Time
          } deriving Show
$( makeLenses ''LogDB )
 
         
emptyLogDB :: IO LogDB
emptyLogDB = LogDB [] [] [] <$> getTime <*> getTime
         
getTime :: IO Time
getTime = do (y,mo,d,h,mi,s) <- toGregorian <$> getCurrentTime
             return (Time (fromIntegral y) mo d h mi s)
        
data Config = Config { waitMicrosecs :: Int, logWaitMicrosecs :: Int }
type Message = (ThreadId, String)


main = run (Config 1000000 100000) 50
      
run :: Config -> Int -> IO ()     
run cfg n = do logChan <- newChan
               logThread <- forkIO $ logger cfg logChan
               chans <- replicateM n newChan
               threads <- (forkIO . worker cfg logChan chans) `mapM` chans
               getLine
               mapM_ killThread (logThread:threads)

logger :: Config -> Chan Log -> IO ()
logger cfg logChan 
  = emptyLogDB >>= newMVar >>= \logDB -> forever $ 
      readChan logChan >>= \log -> 
         forkIO $ modifyMVar_ logDB $
                   \logDB -> do time <- getTime
                                putStr ("Log time: ")
                                print (log ^. loggingDate.sec)
                                putStr ("Current time: ")
                                print (time ^. sec)
                                threadDelay (logWaitMicrosecs cfg)        
                                return $ logDB & debugInfos %~ (log:)
                                               & lastLogDate .~ time 
        
worker :: Config -> Chan Log -> [Chan Message] -> Chan Message -> IO ()
worker cfg logChan chans myChan 
  = forever $
      do thrId <- myThreadId
         partner <- evalRandIO (fromList (map (,1) chans))
         writeChan partner (thrId, "Yolo")
         (senderId, msg) <- readChan myChan
         time <- getTime
         writeChan logChan (Log thrId (msg ++ " from " ++ show senderId) time)
         threadDelay (waitMicrosecs cfg)