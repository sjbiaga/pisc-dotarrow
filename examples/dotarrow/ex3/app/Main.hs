module Main where

import System.Exit (exitFailure)     --  dotarrow
import System.IO (hPutStrLn, stderr) --  dotarrow
import Inp_gUgVwYdD8r                --  dotarrow
import Out_gUgVwYdD8r                --  dotarrow
import Data.List (intercalate)       --  dotarrow


data C = C { m :: Int, n :: Int, p :: Int }
data D = D { q :: Int, r :: Int }


newtype Encoding = Encoding [Integer]

instance Num Encoding where
    fromInteger n = Encoding [n]
    (Encoding lhs) - (Encoding rhs) = Encoding (zipWith (-) lhs rhs)
    negate (Encoding lhs) = Encoding (reverse lhs)
    (Encoding lhs) + (Encoding rhs) = Encoding (zipWith (+) lhs rhs)
    (Encoding lhs) * (Encoding rhs) = Encoding (lhs ++ rhs)
    abs (Encoding lhs) = Encoding (map abs lhs)
    signum (Encoding lhs) = Encoding (map signum lhs)

instance Show Encoding where
    show (Encoding ns) = intercalate " * " (map show ns)

fromInt :: Int -> Encoding
fromInt n = fromInteger (toInteger n)


toC :: Int -> C
toC _ = undefined

toC' :: Encoding -> C
toC' (Encoding [m, n, p]) = C (fromInteger m) (fromInteger n) (fromInteger p)

toD :: Int -> D
toD _ = undefined

toD' :: Encoding -> D
toD' (Encoding [q, r]) = D (fromInteger q) (fromInteger r)


main :: IO ()
main = do
    x <- pure $ -(1 * 2 * 3) + 4 * 5 * 6 - 1 * 1 * 1
    let C a b c = toC x
    y <- pure $ a * b + c * 7
    let D d e = toD y
    z :: Int <- pure (a + b + d - c * e)
    _u :: () <- print z
    return ()
