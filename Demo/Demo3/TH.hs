{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Demo3.TH where

import Control.Applicative
import Control.Monad
import Control.Reference
import Control.Reference.Examples.TH
import Language.Haskell.TH
import Data.Maybe
import Data.Char

import Demo3.Distributed
import Demo3.DerivedRepresentation

makeDerivedRefs :: Name -> Q [Dec]
makeDerivedRefs n 
  = reify n >>= \case
       TyConI decl -> case decl of
         DataD ctx tyConName _ cons _ -> case cons of
           [con] -> makeDerivedLensesForCon tyConName con 
           _ -> fail "makeDerivedRefs: Not a normal record"
         _ -> fail "makeDerivedRefs: Unsupported data type"
       _ -> fail "makeDerivedRefs: Expected the name of a data type or newtype"
    
makeDerivedLensesForCon :: Name -> Con -> Q [Dec]
makeDerivedLensesForCon tyName (RecC conName conFields) 
  = concat <$> sequence (zipWith (\(n, _, t) i -> createDerivedLensForField tyName n t i) conFields [1..])
makeDerivedLensesForCon _ _ = return []

createDerivedLensForField :: Name -> Name -> Type -> Int -> Q [Dec]
createDerivedLensForField typName fldName fldTyp fldInd 
  = do (lensBody, refType) <- genLensBody
       return [ SigD lensName
                     (ConT ''Simple `AppT` ConT ''IOLens 
                                    `AppT` (ConT ''Distributed `AppT` ConT typName)
                                    `AppT` refType)
              , ValD (VarP lensName) (NormalB $ lensBody) []
              ] 
   where lensName = derivedRefName fldName
   
         genLensBody :: Q (Exp, Type)
         genLensBody 
           = do (handler, refType) <- lookupDistrHandler fldTyp
                return ( InfixE (Just $ VarE (mkName ("_" ++ show fldInd))) 
                                (VarE '(&)) 
                                (Just $ VarE handler)
                       , refType)

-- | Gets the reference part that should be used according to 
-- the configuration of the type and the type of the accessed data
lookupDistrHandler :: Type -> Q (Name, Type)
lookupDistrHandler t
  = lookupDistrType t >>=
       maybe (fail $ "Configuration for " ++ pprint t 
                         ++ " is not a valid Node") 
             return
         . (flip lookup [ (''Clean,     ('_clean, t)        )
                        , (''Noth,      ('_noth, distrT)    )
                        , (''Lock,      ('_lock, distrT)    )
                        , (''Multi,     ('_multi, distrT))
                        , (''Fair,      ('_fair, distrT)    )
                        ])
  where distrT = ConT ''Distributed `AppT` t
       
-- | Gets the name of the distribution node ('Lock', 'Lift', ...)
lookupDistrType :: Type -> Q Name
lookupDistrType t
  = do FamilyI _ insts <- reify ''Node
       checkOnlyOneConfig $ catMaybes $ map matchInstance insts
  where matchInstance (TySynInstD _ (TySynEqn [x] (ConT v))) 
          | x == t = Just v
        matchInstance _ = Nothing
             
        checkOnlyOneConfig cfgs = case cfgs of
          [] -> fail ("No configuration found for " ++ pprint t)
          [c] -> return c
          _ -> fail ("Multiple configuration found for " ++ pprint t)
             
derivedRefName :: Name -> Name
derivedRefName = nameBaseStr .- (\case '_':xs -> xs; xs -> '_':xs)

-------------------------------------


makeDistrCons :: ToQType a => a -> Q [Dec]
makeDistrCons a
  = do (n, typArgs) <- getTypeCon <$> toQType a
       reify n >>= \case
         TyConI decl -> case ( decl ^? definedName
                             , decl ^? definedConstructors
                             , decl ^* definedTypeArgs&traverse&typeVarName ) of
           (Just tyConName, Just cons, tyConArgs) -> do
             let translateTV :: Type -> Type
                 translateTV (VarT tv) = fromMaybe (VarT tv) $ lookup tv (zip tyConArgs typArgs)
             concat <$> mapM (makeOneConsDistr tyConName typArgs (length cons)) 
                             ( traverse&_2&conTypes&typeVariables *- translateTV $ zip [1..] cons)
           _ -> fail "makeDerivedRefs: Unsupported data type"
         _ -> fail "makeDerivedRefs: Expected the name of a data type or newtype"
      -- >>= \ret -> runIO (putStrLn (pprint ret)) >> return ret
       
getTypeCon :: Type -> (Name, [Type])
getTypeCon (a `AppT` t) = _2 .- (t:) $ getTypeCon a
getTypeCon (ConT n) = (n,[])
getTypeCon ListT = (''[],[])
       
makeOneConsDistr :: Name -> [Type] -> Int -> (Int,Con) -> Q [Dec]
makeOneConsDistr typName typArgs numCons (ind,con)
  = do conTyp <- makeConType typName typArgs (con ^. conFields)
       clause <- createDistrClause numCons ind (con ^. conName) (con ^. conFields)
       return [ SigD (derivedConName (con ^. conName)) conTyp
              , FunD (derivedConName (con ^. conName)) [clause]
              ]
    
derivedConName :: Name -> Name
derivedConName = nameBaseStr .- (\case "[]"                     -> "%<>"
                                       xs | all isAlphaNum xs   -> "_"++xs
                                          | otherwise           -> "%"++xs)

createDistrClause :: Int -> Int -> Name -> [(Strict, Type)] -> Q Clause
createDistrClause numCons conInd conName argTyps
  = do args <- replicateM (length argTyps) (newName "a")
       distrTypes <- mapM lookupConHandler (map snd argTyps)
       distrVars <- replicateM (length argTyps) (newName "d")
       let distrStmts = zipWith3 (\v d a -> BindS (VarP v) (d `AppE` VarE a)) 
                                 distrVars (map fst distrTypes) args
           retStmt = NoBindS (VarE 'return `AppE` (altCon (foldl AppE memCon (map VarE distrVars))))
       return $ Clause (map VarP args) (NormalB (DoE (distrStmts ++ [retStmt]))) []
  where altCon = if numCons == 1 then id else AppE $ ConE (mkName $ "Alt" ++ show numCons ++ "_" ++ show conInd)
        memCon = ConE (mkName $ "Mem" ++ show (length argTyps))
       
       
-- | Gets the function that can create part of the distributed representation
lookupConHandler :: Type -> Q (Exp, Type)
lookupConHandler t 
  = lookupDistrType t >>=
       maybe (fail $ "Configuration for " ++ pprint t 
                         ++ " is not a valid Node") 
             return
         . (flip lookup [ (''Clean,     (VarE 'newClean, t)        )
                        , (''Noth,      (VarE 'newNoth, distrT)    )
                        , (''Lock,      (VarE 'newLock, distrT)    )
                        , (''Multi,     (VarE 'newMulti, distrT)   )
                        , (''Fair,      (VarE 'newFair, distrT)    )
                        ])
  where distrT = ConT ''Distributed `AppT` t
           
makeConType :: Name -> [Type] -> [(Strict, Type)] -> Q Type
makeConType resTypName resTypArgs argTypes 
  = do distrArgTyps <- mapM (lookupConHandler . snd) argTypes
       return $ foldr1 (\tf -> AppT (AppT ArrowT tf))
                       ( map snd distrArgTyps
                          ++ [ AppT (ConT ''IO) 
                             $ AppT (ConT ''Distributed) 
                             $ foldl AppT (ConT resTypName) resTypArgs ])

varType :: TyVarBndr -> Type
varType (PlainTV n) = VarT n
varType (KindedTV n _) = VarT n
    
    