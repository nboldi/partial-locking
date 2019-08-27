{-# LANGUAGE DefaultSignatures
           , DeriveGeneric
           , TypeOperators
           , RankNTypes
           , FlexibleContexts
           , FlexibleInstances
           , TypeFamilies
           , UndecidableInstances
           , OverlappingInstances
           , MultiParamTypeClasses
           , FunctionalDependencies
           , TemplateHaskell
           , StandaloneDeriving
           #-}

module Demo3.Distributed where

import GHC.Generics
import Control.Concurrent
import Control.Applicative
import Control.Reference
import Control.Monad
import Demo3.DerivedRepresentation

import Data.IORef
import TupleArithm.TH
import TupleArithm.Class
import Language.Haskell.TH
import Unsafe.Coerce
import Debug.Trace

type Distributed a = Flat (Gen (Rep a))
class (Generic a, Gentype (Rep a), Flatten (Gen (Rep a))) => DerivingDistributed a
instance (Generic a, Gentype (Rep a), Flatten (Gen (Rep a))) => DerivingDistributed a

distribute :: (DerivingDistributed a) => a -> IO (Distributed a)
distribute = liftA flatten . distr . from

--  TODO: add (NodeKind (Node a)) constraint
type family Node a :: * -> *

newtype Clean a
  = Clean { _fromClean :: a } 
  deriving (Generic)
  
newtype Noth a
  = Noth { _fromNoth :: Distributed a }
  deriving (Generic)
  
newtype Lock a
  = Lock { _fromLock :: MVar (Distributed a) }
  deriving (Generic)
  
data Multi a
  = Multi { _multiGravel :: MultiGravel
          , _multiData :: IORef (Distributed a)
          }
  deriving (Generic)    
  
data Fair a
  = Fair { _fairGravel :: FairGravel
         , _fairData :: IORef (Distributed a)
         }
  deriving (Generic)  

data MultiGravel 
  = MultiGravel { multiGrCounter :: MVar Int  -- ^ number of readers currently working on the data
                , multiGrWrite :: MVar ()     -- ^ write lock, could be held by one writer or a set of readers
                }

data FairGravel 
  = FairGravel { fairGrCounter :: MVar Int  -- ^ number of readers currently working on the data
               , fairGrReadEntry :: MVar () -- ^ limit, when readers can enter to stop readers from starving out writers
               , fairGrWrite :: MVar ()     -- ^ write lock, could be held by one writer or a set of readers
               }

-----------------------------               

newClean :: a -> IO (Clean a)
newClean = return . Clean

newNoth :: Distributed a -> IO (Noth a)
newNoth = return . Noth

newLock :: Distributed a -> IO (Lock a)
newLock a = Lock <$> newMVar a

newMulti :: Distributed a -> IO (Multi a)
newMulti a = Multi <$> newMultiGravel <*> newIORef a

newFair :: Distributed a -> IO (Fair a)
newFair a = Fair <$> newFairGravel <*> newIORef a
             
------------------------------
             
newMultiGravel :: IO MultiGravel
newMultiGravel = MultiGravel <$> newMVar 0 <*> newMVar ()

multiGrHoldRead :: MultiGravel -> IO ()
multiGrHoldRead gr 
  = modifyMVar_ (multiGrCounter gr) 
                (\cnt -> do when (cnt == 0) (takeMVar (multiGrWrite gr))
                            return (cnt+1))

multiGrRelRead :: MultiGravel -> IO ()
multiGrRelRead gr 
  = modifyMVar_ (multiGrCounter gr) 
                (\cnt -> do when (cnt == 1) $ putMVar (multiGrWrite gr) ()
                            return (cnt-1))

multiGrHoldWrite :: MultiGravel -> IO ()
multiGrHoldWrite gr 
  = takeMVar (multiGrWrite gr)

multiGrRelWrite :: MultiGravel -> IO ()
multiGrRelWrite gr 
  = putMVar (multiGrWrite gr) ()

---------------------------------  
  
newFairGravel :: IO FairGravel
newFairGravel = FairGravel <$> newMVar 0 <*> newMVar () <*> newMVar ()

fairGrHoldRead :: FairGravel -> IO ()
fairGrHoldRead gr 
  = do readMVar (fairGrReadEntry gr) 
       modifyMVar_ (fairGrCounter gr) 
                   (\cnt -> do when (cnt == 0) (takeMVar (fairGrWrite gr))
                               return (cnt+1))

fairGrRelRead :: FairGravel -> IO ()
fairGrRelRead gr 
  = modifyMVar_ (fairGrCounter gr) 
                (\cnt -> do when (cnt == 1) $ putMVar (fairGrWrite gr) ()
                            return (cnt-1))

fairGrHoldWrite :: FairGravel -> IO ()
fairGrHoldWrite gr 
  = withMVar (fairGrReadEntry gr)
      $ const $ takeMVar (fairGrWrite gr)

fairGrRelWrite :: FairGravel -> IO ()
fairGrRelWrite gr 
  = putMVar (fairGrWrite gr) ()
  
  -----------------------------

class NodeKind (kind :: * -> *) where
    distr' :: (DerivingDistributed a) => a -> IO (kind a)

instance NodeKind Clean where
    distr' = newClean
    
instance NodeKind Noth where
    distr' = distribute >=> newNoth
    
instance NodeKind Lock where
    distr' = distribute >=> newLock

instance NodeKind Multi where
    distr' = distribute >=> newMulti

instance NodeKind Fair where
    distr' = distribute >=> newFair

class Gentype (a :: * -> *) where
    type Gen a :: *
    distr :: a f -> IO (Gen a)

-- -- U1
instance Gentype U1 where
    type Gen U1 = Mem0
    distr _ = return Mem0
  
-- -- a :+: b
instance (Gentype a, Gentype b) => Gentype (a :+: b) where
    type Gen (a :+: b) = Alt2 (Gen a) (Gen b)
    distr (L1 a) = liftA Alt2_1 (distr a)
    distr (R1 b) = liftA Alt2_2 (distr b)


-- -- a :*: b
instance (Gentype a, Gentype b) => Gentype (a :*: b) where
    type Gen (a :*: b) = (Mem2 (Gen a) (Gen b))
    distr (a :*: b) = Mem2 <$> distr a <*> distr b


-- -- C1 c a
instance (Gentype a, Constructor c) => Gentype (C1 c a) where
    type Gen (C1 c a) = Gen a
    distr (M1 a) = distr a


-- -- D1 c a
instance (Gentype a, Datatype c) => Gentype (D1 c a) where
    type Gen (D1 c a) = Gen a
    distr (M1 a) = distr a


-- -- S1 c a
instance (Gentype a, Selector c) => Gentype (S1 c a) where
    type Gen (S1 c a) = Gen a
    distr (M1 a) = distr a


-- -- K1 c a
instance (NodeKind (Node a), DerivingDistributed a) => Gentype (K1 c a) where
    type Gen (K1 c a) = Mem1 (Node a a)
    distr (K1 a) = liftA Mem1 (distr' a)

-- * Creating references



$(makeReferences ''Clean)
  
_clean :: Simple Lens (Clean a) a
_clean = fromClean

$(makeReferences ''Noth)
  
_noth :: Simple Lens (Noth a) (Distributed a)
_noth = fromNoth

$(makeReferences ''Lock)
  
_lock :: Simple IOLens (Lock a) (Distributed a)
_lock = fromLock & mvar

$(makeReferences ''Fair)

_fair :: Simple IOLens (Fair a) (Distributed a)
_fair = referenceWithClose 
          (\(Fair gr a) -> morph (fairGrHoldRead gr *> readIORef a)) 
          (morph . fairGrRelRead . _fairGravel) 
          (\b fair@(Fair gr a) -> morph (fairGrHoldWrite gr *> writeIORef a b) *> return fair)
          (morph . fairGrRelWrite . _fairGravel)
          (\trf fair@(Fair gr a) -> morph (fairGrHoldRead gr *> readIORef a)
                                        >>= trf >>= morph . writeIORef a
                                        >> return fair) 
          (morph . fairGrRelRead . _fairGravel)
          
_fair' :: Simple IOLens (Fair a) (Distributed a)
_fair' = referenceWithClose 
          (\(Fair gr a) -> morph (fairGrHoldRead gr *> readIORef a)) 
          (morph . fairGrRelRead . _fairGravel) 
          (\b fair@(Fair gr a) -> morph (fairGrHoldWrite gr *> writeIORef a b) *> return fair)
          (morph . fairGrRelWrite . _fairGravel)
          (\trf fair@(Fair gr a) -> morph (fairGrHoldWrite gr *> readIORef a)
                                        >>= trf >>= morph . writeIORef a
                                        >> return fair) 
          (morph . fairGrRelWrite . _fairGravel)

breakFair :: Simple IOLens (Fair a) (Distributed a)
breakFair = fairData & ioref
          
$(makeReferences ''Multi)

_multi :: Simple IOLens (Multi a) (Distributed a)
_multi = referenceWithClose 
          (\(Multi gr a) -> morph (multiGrHoldRead gr *> readIORef a)) 
          (morph . multiGrRelRead . _multiGravel) 
          (\b multi@(Multi gr a) -> morph (multiGrHoldWrite gr *> writeIORef a b) *> return multi)
          (morph . multiGrRelWrite . _multiGravel)
          (\trf multi@(Multi gr a) -> morph (multiGrHoldRead gr *> readIORef a)
                                        >>= trf >>= morph . writeIORef a
                                        >> return multi) 
          (morph . multiGrRelRead . _multiGravel)
          
_multi' :: Simple IOLens (Multi a) (Distributed a)
_multi' = referenceWithClose 
          (\(Multi gr a) -> morph (multiGrHoldRead gr *> readIORef a)) 
          (morph . multiGrRelRead . _multiGravel) 
          (\b multi@(Multi gr a) -> morph (multiGrHoldWrite gr *> writeIORef a b) *> return multi)
          (morph . multiGrRelWrite . _multiGravel)
          (\trf multi@(Multi gr a) -> morph (multiGrHoldWrite gr *> readIORef a)
                                        >>= trf >>= morph . writeIORef a
                                        >> return multi) 
          (morph . multiGrRelWrite . _multiGravel)

breakMulti :: Simple IOLens (Multi a) (Distributed a)
breakMulti = multiData & ioref
