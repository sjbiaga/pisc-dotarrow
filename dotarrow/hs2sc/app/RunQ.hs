{-# LANGUAGE TemplateHaskell #-}
module RunQ where

import Language.Haskell.TH.Syntax

import HaScalaM.Types.Tilde


src :: Quasi q => q Exp
src = runQ [| (\(imp' :: SmStat
                ,etc :: [SmStat]
                ,obj' :: SmStat
                ) ->
                SmSource ([imp'] ++ etc ++ [obj'])
              ) |]


imp :: Quasi q => q Exp
imp = runQ [| SImpExp (SmImportS [SmImporter (RTSelect (SmSelectRT (TRef (RTName (SmNameT "cats")))
                                                                   (SmNameT "effect")))
                                             [SmNameI (NName (SmIndeterminateN "IO"))
                                             ,SmNameI (NName (SmIndeterminateN "IOApp"))]])
           |]

dec :: Quasi q => q Exp
dec = runQ [| (\(name :: String
                ,it' :: [SmParamT SmMod SmName SmType' SmTerm]
                ) ->
                SClass (SmClassS [MMod SmCaseM]
                                 (SmNameT' name)
                                 (SmParamClauseT' [])
                                 (SmCtorPrimary [] (NName SmAnonymousN) [SmParamClauseT it' Nothing ])
                                 (SmTemplate [] [] (SmSelf (NName SmAnonymousN) Nothing) [] []))
               ) |]

var :: Quasi q => q Exp
var = runQ [| (\(name :: String
                ,type' :: String
                ) ->
                SmParamT []
                         (NTName (SmNameT name))
                         (Just (T'Ref (RT'Name (SmNameT' type'))))
                         Nothing
              ) |]

app :: Quasi q => q Exp
app = runQ [| (\(def' :: SmStat) ->
                SObject (SmObjectS []
                                   (SmNameT "App")
                                   (SmTemplate []
                                               [SmInit (T'Ref (RT'Ref (SmSelectRT' (RTName (SmNameT "IOApp"))
                                                                                   (SmNameT' "Simple"))))
                                                        (NName SmAnonymousN)
                                                        []
                                               ]
                                               (SmSelf (NName SmAnonymousN) Nothing)
                                               [def']
                                               []
                                   )
                        )
              ) |]

def :: Quasi q => q Exp
def = runQ [| (\(gen' :: [SmEnumerator]) ->
                SDef (SmDefS [MMod SmOverrideM]
                             (SmNameT "run")
                             []
                             (Just (T'Apply (SmApplyT' (T'Ref (RT'Name (SmNameT' "IO")))
                                                       (SmArgClauseT' [T'Ref (RT'Name (SmNameT' "Unit"))]))))
                             (TForYield (SmForYieldT gen' (TLit SmUnitL))))
              ) |]

gn1 :: Quasi q => q Exp
gn1 = runQ [| (\(name :: String
                ,exp' :: SmStat) ->
                EGenerator (SmGeneratorE (PPat (SmTypedP (PVar (SmVarP (SmNameT name)))
                                                         (T'Ref (RT'Name (SmNameT' "Unit")))))
                                         (TApply (SmApplyT (TRef (RTName (SmNameT "IO")))
                                                           (SmArgClauseT [TTerm (SmBlockT [exp'])] Nothing))))
              ) |]

gn2 :: Quasi q => q Exp
gn2 = runQ [| (\(name :: String
                ,name' :: String
                ,it' :: [SmTerm]
                ) ->
                EGenerator (SmGeneratorE (PVar (SmVarP (SmNameT name)))
                                         (TApply (SmApplyT (TRef (RTName (SmNameT "IO")))
                                                           (SmArgClauseT [TTerm (SmBlockT [STerm (TApply (SmApplyT (TRef (RTName (SmNameT name')))
                                                                                                                   (SmArgClauseT it' Nothing)))
                                                                                          ])] Nothing))))
              ) |]

gn3 :: Quasi q => q Exp
gn3 = runQ [| (\(name :: String
                ,name' :: String
                ,it' :: [SmPat]
                ) ->
                EVal (SmValE (PExtract (SmExtractP (TRef (RTName (SmNameT name')))
                                                   (SmArgClauseP it')))
                             (TRef (RTName (SmNameT name))))
              ) |]

gn4 :: Quasi q => q Exp
gn4 = runQ [| (\(name :: String
                ,type' :: String
                ,exp' :: SmStat
                ) ->
                EGenerator (SmGeneratorE (PPat (SmTypedP (PVar (SmVarP (SmNameT name)))
                                                         (T'Ref (RT'Name (SmNameT' type')))))
                                         (TApply (SmApplyT (TRef (RTName (SmNameT "IO")))
                                                           (SmArgClauseT [TTerm (SmBlockT [exp'])] Nothing))))
              ) |]

gn5 :: Quasi q => q Exp
gn5 = runQ [| EGenerator (SmGeneratorE (PPat SmWildcardP)
                                       (TRef (RTSelect (SmSelectRT (TApply (SmApplyT (TRef (RTName (SmNameT "IO")))
                                                                                       (SmArgClauseT [TTerm (SmBlockT [])] Nothing)))
                                                                   (SmNameT "void")))))
           |]
