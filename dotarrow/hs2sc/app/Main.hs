{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified RunQ as Q
import qualified Stat as S
import qualified Term as T

import Data.Text (pack, replace, unpack)
import Language.Haskell.TH.Ppr hiding (ppr)
import Language.Haskell.Meta (parseDecs)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH (pprint)
import System.Environment (getArgs)

import HaScalaM.Instances.Base
import HaScalaM.Instances.Enums
import HaScalaM.Instances.Pat
import HaScalaM.Instances.Ref
import HaScalaM.Instances.Stat
import HaScalaM.Instances.Term
import HaScalaM.Instances.Type
import HaScalaM.Instances.Tilde
import HaScalaM.Instances.Show
import HaScalaM.Types.Base
import HaScalaM.Types.Enums
import HaScalaM.Types.Pat
import HaScalaM.Types.Ref
import HaScalaM.Types.Stat
import HaScalaM.Types.Term
import HaScalaM.Types.Type
import HaScalaM.Types.Tilde

ppr :: Exp -> String
ppr = unpack . replace "GHC.Num." "" . pack . pprint

src :: Quasi q => q SmStat -> [SmStat] -> q SmStat -> q (SmSource SmStat)
src imp etc obj = do
                  imp' <- imp
                  obj' <- obj
                  return $ $(Q.src) (imp', etc, obj')

imp :: Quasi q => q SmStat
imp = return $ $(Q.imp)

dec :: Quasi q => Dec -> q SmStat
dec (DataD [] (Name (OccName name) _) [] Nothing [RecC _ it] []
    ) = do
        it' <- traverse var it
        return $ $(Q.dec) (name, it')
dec _ = error "dec"

var :: Quasi q => VarBangType -> q (SmParamT SmMod SmName SmType' SmTerm)
var (Name (OccName name) _
    ,Bang NoSourceUnpackedness NoSourceStrictness
    ,ConT (Name (OccName type') _)
    ) = return $ $(Q.var) (name, type')
var _ = error "var"

app :: Quasi q => [Dec] -> q SmStat
app [SigD (Name (OccName "main") _)
          (AppT (ConT (Name (OccName "IO") _))
                (ConT (Name (OccName "()") _)))
    ,ValD (VarP (Name (OccName "main") _))
          (NormalB (DoE Nothing stmts))
          []
    ] = do
        def' <- def stmts
        return $ $(Q.app) def'
app _ = error "app"

def :: Quasi q => [Stmt] -> q SmStat
def stmts = do
            gen' <- traverse gen stmts
            return $ $(Q.def) gen'

gen :: Quasi q => Stmt -> q SmEnumerator
gen (BindS (SigP (VarP (Name (OccName name) _)) (ConT (Name (OccName "()") _)))
           (AppE (VarE (Name (OccName "print") _)) it)
    ) = do
        exp' <- $(S.exp) ("println(" ++ ppr it ++ ")")
        return $ $(Q.gn1) (name, exp')
gen (BindS (VarP (Name (OccName name) _))
           (UInfixE (VarE (Name (OccName "pure") _))
                    (VarE (Name (OccName "$") _))
                    it
           )
    ) = do
        (name', it') <- gen' it
        return $ $(Q.gn2) (name, name', it')
gen (LetS [ValD (ConP (Name (OccName name') _) [] it) (NormalB (VarE (Name (OccName name) _))) []]
    ) = do
        let it' = map pat it
        return $ $(Q.gn3) (name, name', it')
    where pat (VarP (Name (OccName name) _)) = PVar (SmVarP (SmNameT name))
gen (BindS (SigP (VarP (Name (OccName name) _))
                 (ConT (Name (OccName type') _)))
           (AppE (VarE (Name (OccName _) _)) it)
    ) = do
        exp' <- $(S.exp) (ppr it)
        return $ $(Q.gn4) (name, type', exp')
gen (NoBindS (AppE (VarE (Name (OccName "return") _)) (ConE (Name (OccName "()") _)))
    ) = return $ $(Q.gn5)
gen _ = error "gen"

gen' :: Quasi q => Exp -> q (String, [SmTerm])
gen' (AppE (ConE (Name (OccName name') _)) it) =
    do
        exp' <- $(T.exp) (ppr it)
        return (name', [exp'])
gen' (AppE ap it) =
    do
        (name', ls) <- gen' ap
        exp' <- $(T.exp) (ppr it)
        return (name', ls ++ [exp'])


main :: IO ()
main = do
       [arg] <- getArgs
       str <- readFile arg
       let Right ds = parseDecs str
       let n = length ds
       etc <- traverse dec (take (n - 2) ds)
       src' <- src imp etc (app (drop (n - 2) ds))
       print src'
       return ()
