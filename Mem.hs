{-# LANGUAGE DefaultSignatures
           , DeriveGeneric
           , TypeOperators
           , FlexibleContexts
           , FlexibleInstances
           , TypeFamilies
           , UndecidableInstances
           , OverlappingInstances
           , MultiParamTypeClasses
           , FunctionalDependencies
           , TemplateHaskell
           #-}

module Mem where

import GHC.Generics
import Control.Concurrent

import TupleArithm_TH
import TupleArithm_Class
import Language.Haskell.TH


$( let mkAlt i = mkName ("Mem" ++ show i) 
    in generateArithmetic ( ArithGenConf 
                             (Just $ mkAlt)
                             (\vars -> foldl AppT (ConT (mkAlt (length vars))) vars) 
                             (\vars -> foldl AppE (ConE (mkAlt (length vars))) vars) 
                             (\vars -> ConP (mkAlt (length vars)) vars) 
                             [''Show]) 6 
 )

instance Flatten (Mem1 a) where
  type Flat (Mem1 a) = Mem1 a
  flatten = id


