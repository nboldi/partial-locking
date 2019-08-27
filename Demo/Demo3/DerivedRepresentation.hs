{-# LANGUAGE DefaultSignatures
           , DeriveGeneric
           , TypeOperators
           , FlexibleContexts
           , FlexibleInstances
           , LambdaCase
           , TypeFamilies
           , UndecidableInstances
           , OverlappingInstances
           , MultiParamTypeClasses
           , FunctionalDependencies
           , TemplateHaskell
           #-}

module Demo3.DerivedRepresentation where

import GHC.Generics
import Control.Concurrent
import Control.Reference
import Control.Reference.TH.Tuple
import Control.Reference.TH.Records
import Control.Applicative

import TupleArithm.TH
import TupleArithm.Class
import Language.Haskell.TH

$( do let mkMem :: Int -> Name
          mkMem i = mkName ("Mem" ++ show i) 
          
          memTyp :: [Type] -> Type
          memTyp vars = foldl AppT (ConT (mkMem (length vars))) vars
          
          memPat :: [Pat] -> Pat
          memPat vars = ConP (mkMem (length vars)) vars
          
          memExp :: [Exp] -> Exp
          memExp vars = foldl AppE (ConE (mkMem (length vars))) vars
      memDecls <- generateArithmetic ( ArithGenConf (Just mkMem) memTyp memExp memPat [] ) 6 
      memTups <- makeTupleRefs (TupleConf (memTyp . map VarT) 
                                          (memPat . map VarP) 
                                          (memExp . map VarE)
                                ) 6 6
      return (memDecls ++ memTups)
  )
 
instance Flatten (Mem1 a) where
  type Flat (Mem1 a) = Mem1 a
  flatten = id
          
instance Lens_1 (Mem1 a) (Mem1 a') a a' where
  _1 = lens (\(Mem1 x) -> x) 
            (\y _ -> Mem1 y)  
  
newtype Alt1 a
    = Alt1_1 { _fromAlt1_1 :: a }
data Alt2 a b
    = Alt2_1 { _fromAlt2_1 :: a }
    | Alt2_2 { _fromAlt2_2 :: b }
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

