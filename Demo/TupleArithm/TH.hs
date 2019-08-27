{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell, TupleSections, NamedFieldPuns #-}

module TupleArithm.TH where

import TupleArithm.Class

import Language.Haskell.TH
import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Data.Maybe

-- | Generates arithmetic for @()@, haskell tuples @(,) (,,) ...@ and a special 1-tuple
generateTupleArithmetic :: Name -> Name -> Int -> Q [Dec]
generateTupleArithmetic oneTupTyp oneTupCtr
  = generateArithmetic (ArithGenConf Nothing tupType tupExpr tupPat [])
  where tupType [typ] = ConT oneTupCtr `AppT` typ
        tupType typs = foldl AppT (TupleT (length typs)) typs
  
        tupExpr [var] = ConE oneTupCtr `AppE` var
        tupExpr vars = TupE vars

        tupPat [var] = ConP oneTupCtr [var]
        tupPat vars = TupP vars
        
data ArithGenConf 
  = ArithGenConf { typeNameGen :: Maybe (Int -> Name) -- ^ generates the name of n-tuple
                 , genType     :: [Type] -> Type -- ^ generates the type of the n-tuple
                 , ctorGen     :: [Exp] -> Exp  
                     -- ^ generates an expression of an n-tuple from the arguments
                 , patternGen  :: [Pat] -> Pat 
                     -- ^ generates a pattern that binds the components of the n-tuple to the given names
                 , toDerive :: [Name]
                 }
        
-- | Frontend function, creates datatypes and instances for a given type names, patterns and expressions.
generateArithmetic :: ArithGenConf -> Int -- ^ maximum tuple size 
                   -> Q [Dec]
generateArithmetic conf n 
  = do addDefs <- runReaderT ( genAddInst `mapM` [ (x, y) | x <- [0..n], y <- [0..n] ] ) conf
       flattenDefs <- runReaderT ( genFlattenInst `mapM` [0,2..n] ) conf
       genDefs <- runReaderT ( genTups (2*n) ) conf
       return $ addDefs ++ flattenDefs ++ genDefs
                  
genTups :: Int -> ReaderT ArithGenConf Q [Dec]
genTups n = catMaybes <$> mapM genTup [0..n]
                    
genTup :: Int -> ReaderT ArithGenConf Q (Maybe Dec)
genTup n 
  = do defsToDerive <- asks toDerive
       tNGen <- asks typeNameGen
       case tNGen of 
         Just f ->
           do typVars <- lift $ replicateM n (newName "t")
              return $ Just $ DataD [] (f n) (map PlainTV typVars) 
                                    [ NormalC (f n) (map ((NotStrict,) . VarT) typVars) ] defsToDerive
         Nothing -> return Nothing
                          

genAddInst :: (Int,Int) -> ReaderT ArithGenConf Q Dec
genAddInst (i,j)
  = do ArithGenConf _ genTupType genTupExpr genTupPat _ <- ask
       iTypParams <- lift $ replicateM i (VarT <$> newName "t")
       jTypParams <- lift $ replicateM j (VarT <$> newName "t")
       let instHead = ConT ''TypeAdd `AppT` genTupType iTypParams `AppT` genTupType jTypParams
           typeFun = TySynInstD addTypFunName 
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 708
                       $ TySynEqn
#endif
                                [genTupType iTypParams, genTupType jTypParams]  
                                (genTupType (iTypParams ++ jTypParams))
       iVars <- lift $ replicateM i (newName "v")
       jVars <- lift $ replicateM j (newName "v")
       let fun = FunD addFunName [ Clause [ genTupPat (map VarP iVars)
                                          , genTupPat (map VarP jVars) 
                                          ] 
                                          ( NormalB (genTupExpr (map VarE (iVars ++ jVars))) ) 
                                          [] ]
       return $ InstanceD [] instHead [typeFun, fun]

addTypFunName = mkName "::+::"
addFunName = mkName "%+%"
                   
genFlattenInst :: Int -> ReaderT ArithGenConf Q Dec
genFlattenInst i 
  = do ArithGenConf _ genTupType genTupExpr genTupPat _ <- ask
       typVars <- lift $ replicateM i (VarT <$> newName "t")
       let flattenCtx = map (ClassP ''Flatten . (:[])) typVars
           flattenedParams = map (AppT (ConT ''Flat)) typVars
           typeAddCtx = zipWith (\s n -> ClassP ''TypeAdd [s,n])
                                ( scanl (\s v -> ConT addTypFunName `AppT` s `AppT` v) (genTupType [])
                                        (init flattenedParams) )
                                flattenedParams
           instHead = ConT ''Flatten `AppT` genTupType typVars
       
           typeFun = TySynInstD ''Flat
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 708
                       $ TySynEqn
#endif
                       [genTupType typVars] 
                       (foldl (\l r -> ConT addTypFunName `AppT` l `AppT` r) 
                              (genTupType []) (map (AppT (ConT ''Flat)) typVars))
       
       vars <- lift $ replicateM i (newName "v")
       let fun = FunD 'flatten [ Clause [genTupPat (map VarP vars)] 
                                        (NormalB (foldl (\l r -> VarE addFunName `AppE` l `AppE` r) 
                                                        (genTupExpr []) 
                                                        ( map (AppE (VarE 'flatten) . VarE) vars ) ) ) 
                                        [] ]
       
       return $ InstanceD (flattenCtx ++ typeAddCtx) instHead [typeFun, fun]

        
        