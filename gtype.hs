

{-# LANGUAGE DeriveGeneric
           , TypeOperators
           , FlexibleContexts
           , FlexibleInstances
           , TypeFamilies
           , UndecidableInstances
           , RankNTypes
           #-}

import GHC.Generics
import Control.Concurrent
import Control.Applicative

import TupleArithm_Class
import Mem

import Debug.Trace

type Distributed a = Flat (Gen (Rep a))

distribute :: (DerivingDistributed a) => a -> IO (Distributed a)
distribute = liftA flatten . distr . from


newtype Lift a
  = Lift (Step (Distributed a))
  deriving (Generic)

newtype Lock a
  = Lock (MVar (Distributed a))
  deriving (Generic)

newtype Noth a
  = Noth (Distributed a)
  deriving (Generic)

newtype LiftLock a
  = LiftLock (Step (MVar (Distributed a)))
  deriving (Generic)

newtype Clean a
  = Clean a
  deriving (Show)

data Step a
  = Step (MVar [Gravel]) a
  deriving (Generic)

type Gravel = ()


class NodeKind (kind :: * -> *) where
    distr' :: forall a . (DerivingDistributed a) => a -> IO (kind a)

instance NodeKind Lock where
    distr' a = Lock <$> ((distribute a) >>= newMVar)

instance NodeKind Lift where
    distr' a = Lift <$> (Step <$> newMVar [] <*> distribute a)

instance NodeKind LiftLock where
    distr' a = LiftLock <$> (Step <$> (newMVar []) <*> (distribute a >>= newMVar))

instance NodeKind Noth where
    distr' a = Noth <$> (distribute a)

instance NodeKind Clean where
    distr' a = return (Clean a)


--  TODO: add (NodeKind (Node a)) constraint
type family Node a :: * -> *

class (Generic a, Gentype (Rep a), Flatten (Gen (Rep a))) => DerivingDistributed a
instance (Generic a, Gentype (Rep a), Flatten (Gen (Rep a))) => DerivingDistributed a


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
instance (NodeKind (Node a), DerivingDistributed a, Show a) => Gentype (K1 c a) where
    type Gen (K1 c a) = Mem1 (Node a a)
    distr (K1 a) = liftA Mem1 (distr' a)


data Alt1 a
    = Alt1_1 a
data Alt2 a b
    = Alt2_1 a
    | Alt2_2 b
data Alt3 a b c
    = Alt3_1 a
    | Alt3_2 b
    | Alt3_3 c
data Alt4 a b c d
    = Alt4_1 a
    | Alt4_2 b
    | Alt4_3 c
    | Alt4_4 d
data Alt5 a b c d e
    = Alt5_1 a
    | Alt5_2 b
    | Alt5_3 c
    | Alt5_4 d
    | Alt5_5 e
data Alt6 a b c d e f
    = Alt6_1 a
    | Alt6_2 b
    | Alt6_3 c
    | Alt6_4 d
    | Alt6_5 e
    | Alt6_6 f

instance (Flatten a, Flatten b) => Flatten (Alt2 a b) where
    type Flat (Alt2 a b) = Alt2 (Flat a) (Flat b)
    flatten (Alt2_1 a) = Alt2_1 (flatten a)
    flatten (Alt2_2 b) = Alt2_2 (flatten b) 


-- -- Sandbox

newtype Store a = Store a
  deriving (Eq, Show, Generic)
type instance Node (Store a) = Lock

newtype Semaphore = Semaphore ()
  deriving (Eq, Show, Generic)
type instance Node Semaphore = Lock

newtype Cleaning a
  = Cleaning a
  deriving (Eq, Show, Generic)
type instance Node (Cleaning a) = Clean


data LogDB
  = LogDB
    { criticalErrors    :: [Log]
    , errors            :: [Log]
    , debugInfos        :: [Log]
    , startDate         :: Time
    , startThreadId     :: Cleaning ThreadId--Int --ThreadId
    }
  deriving (Eq, Show, Generic)

data Log
  = Log
    { loggerThread      :: Cleaning ThreadId--Int --ThreadId
    , msg               :: String
    , loggingDate       :: Time
    }
  deriving (Eq, Show, Generic)

data Time
  = Time
    { year    :: Int
    , mounth  :: Int
    , day     :: Int
    , hour    :: Int
    , minute  :: Int
    , sec     :: Int
    }
  deriving (Eq, Show, Generic)

type instance Node Time = Lock
type instance Node ThreadId = Lock
type instance Node Log = LiftLock
type instance Node [Log] = Lift
type instance Node String = Clean
type instance Node Char = Clean
type instance Node Int = Clean


-- TODO: Clean solution for Generic ThreadId problem
-- Its a technical (hack) instance
-- This functions (from, to) will not run
instance Generic ThreadId where
    type Rep ThreadId = K1 ThreadId ThreadId
    from = K1
    to (K1 a) = a

initLogDB :: IO (Distributed LogDB)
initLogDB = do
    th <- myThreadId
    distribute emptyLogDB { startThreadId = Cleaning th }

emptyLogDB :: LogDB
emptyLogDB
  = LogDB
    { criticalErrors    = []
    , errors            = []
    , debugInfos        = []
    , startDate         = Time 0 0 0 0 0 0
    , startThreadId     = undefined
    }


