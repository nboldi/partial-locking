{-# LANGUAGE TypeOperators, TypeFamilies, MultiParamTypeClasses, FlexibleContexts
           , FlexibleInstances, UndecidableInstances #-}
           
module TupleArithm.Class where

class TypeAdd a b where
  type a ::+:: b :: *
  (%+%) :: a -> b -> a ::+:: b
  
class Flatten a where
  type Flat a :: *
  flatten :: a -> Flat a
