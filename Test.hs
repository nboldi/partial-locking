{-# LANGUAGE TypeOperators, TypeFamilies, MultiParamTypeClasses, FlexibleContexts, TemplateHaskell
           , FlexibleInstances, UndecidableInstances, StandaloneDeriving #-}
           
module Test where

import TupleArithm_Class
import TupleArithm_TH
import Language.Haskell.TH
  
$( let mkAlt i = mkName ("Alt" ++ show i) 
    in generateArithmetic ( ArithGenConf 
                             (Just $ mkAlt)
                             (\vars -> foldl AppT (ConT (mkAlt (length vars))) vars) 
                             (\vars -> foldl AppE (ConE (mkAlt (length vars))) vars) 
                             (\vars -> ConP (mkAlt (length vars)) vars) 
                             [''Show]) 2 
 )

-- $( generateTupleArithmetic ''T 'T 3 )


-- $( reify (mkName "dfasf") )

-- instance Flatten () where
  -- type Flat () = ()
  -- flatten = id
  
instance Flatten (Alt1 a) where
  type Flat (Alt1 a) = Alt1 a
  flatten = id

test2 = flatten $ Alt2 (Alt1 1) (Alt2 (Alt1 2) (Alt1 2))
  
-- test1 = flatten ((T 1, T 'c'), T 3.14)
  
-- test = flatten ( ( T 4
                 -- , ( T ( T () )
                   -- , ( T ()
                     -- , ( T ""
                       -- , T 'a'
                       -- )
                     -- )
                     -- , T []
                     -- , T 2)
                 -- )
               -- , ( T 'C'
                 -- , T ()
                 -- , T ((),())
                 -- ) 
               -- )
  