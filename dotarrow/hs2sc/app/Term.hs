{-# LANGUAGE TemplateHaskell #-}
module Term where

import Data.ByteString.Lazy.Char8 (unpack)
import Data.Functor
import Language.Haskell.TH.Syntax
import System.Process.Typed

import HaScalaM.Types.Tilde (SmTerm (T''))

amm :: Quasi q => String -> q String
amm exp =
       do
          (out, _) <- readProcess_ (proc "amm" ["-c", i ++ i2 ++ pr])
          return (unpack out)
    where i = "import $ivy.`org.scalameta:scalameta_2.13:4.9.5`;"
          i2 = "import scala.meta._; import dialects.Scala3;"
          pr = "print(\"\"\"" ++ exp ++ "\"\"\".parse[Term].get.structure)"

exp :: Quasi q => q Exp
exp = runQ [| \(it::String) -> amm it <&> T'' |]
