module Main where

import Prelude hiding (exp)

import Data.List (intercalate)       --  dotarrow
import System.Exit (exitFailure)     --  dotarrow
import System.IO (hPutStrLn, stderr) --  dotarrow
import Fun                           --  dotarrow
import Inp_gUgVwYdD8r                --  dotarrow
import Out_gUgVwYdD8r                --  dotarrow


data C = C { m :: Int, n :: Int, p :: Int }
data D = D { q :: Int, r :: Int }


data Op = Add
        | Sub
        | Mul

-- instance Show Op where
--     show Add = "+"
--     show Sub = "-"
--     show Mul = "*"

data Expr = Val { number :: Integer }
          | Fun { args :: [Expr] }
          | Bin { lhs :: Expr, op :: Op, rhs :: Expr }
          | Neg { exp :: Expr }

-- instance Show Expr where
--     show (Val n) = show n
--     show (Neg (Val 0)) = "0"
--     show (Neg (Val n)) | n > 0 = "-" ++ show n
--     show (Neg e) = "-(" ++ show e ++ ")"
--     show (Bin l o r) = l' ++ " " ++ show o ++ " " ++ r'
--         where l' = case l of (Val n)   -> show n
--                              f@(Fun _) -> show f
--                              _         -> "(" ++ show l ++ ")"
--               r' = case r of (Val n)   -> show n
--                              f@(Fun _) -> show f
--                              _         -> "(" ++ show r ++ ")"
--     show (Fun (Val f:as)) = n ++ "(" ++ intercalate ", " (map show as) ++ ")"
--         where n = case fromInteger f
--                   of -1 -> "ack"
--                      -2 -> "fib"
--                      -3 -> "fac"
--                      -4 -> "sud"

eval :: Expr -> Integer
eval (Val n) = n
eval (Bin l o r) = case o of
                     Add -> l' + r'
                     Sub -> l' - r'
                     Mul -> l' * r'
    where l' = eval l
          r' = eval r
eval (Neg r) = 0 - eval r
eval (Fun [Val f, m, n])
    | (-1) == fromInteger f = ack m' n'
    where m' = eval m
          n' = eval n
eval (Fun [Val f, n]) =
    case fromInteger f
    of (-2) -> fib n'
       (-3) -> fac n'
    where n' = eval n
eval (Fun [Val f, m, n, p])
    | (-4) == fromInteger f = sud m' n' p'
    where m' = eval m
          n' = eval n
          p' = eval p


newtype Encoding = Encoding (Either Expr [Expr])

instance Num Encoding where
    fromInteger n = Encoding (Right [Val n])
    lhs - rhs = bin lhs Sub rhs
    negate (Encoding rhs) =
        case rhs of
          Left l                     -> Encoding (Left (Neg l))
          Right [r@(Val n)] | n >= 0 -> Encoding (Left (Neg r))
    lhs + rhs = bin lhs Add rhs
    lhs * rhs =
        case (lhs', rhs') of
          (Right l, Right r) -> Encoding (fun (l ++ r))
          (Right l, Left r)  ->
              case fun l of
                Right it     -> Encoding (fun (it ++ [r]))
                Left it      -> Encoding (Left (Bin it Mul r))
          (Left l, Right r)  ->
              case fun r of
                Right it     -> undefined
                Left it      -> Encoding (Left (Bin l Mul it))
          _                  -> bin lhs Mul rhs
        where Encoding lhs' = lhs
              Encoding rhs' = rhs
    abs (Encoding lhs) =
        case lhs of
          Left l            -> Encoding (Left (Val (abs (eval l))))
          Right [r@(Val _)] -> Encoding (Left (Val (abs (eval r))))
    signum (Encoding lhs) =
        case lhs of
          Left l            -> Encoding (Left (Val (signum (eval l))))
          Right [r@(Val _)] -> Encoding (Left (Val (signum (eval r))))
bin :: Encoding -> Op -> Encoding -> Encoding
bin lhs op rhs =
    case (lhs', rhs') of
      (Left l, Left r)                                -> Encoding (Left (Bin l op r))
      (Right [l@(Val n)], Right [r@(Val _)]) | n >= 0 -> Encoding (Left (Bin l op r))
      (Right [l@(Val n)], Left r) | n >= 0            -> Encoding (Left (Bin l op r))
      (Left l, Right [r@(Val _)])                     -> Encoding (Left (Bin l op r))
    where Encoding lhs' = lhs
          Encoding rhs' = rhs
fun :: [Expr] -> Either Expr [Expr]
fun (Val f : _ : _ : _) | f == -2 || f == -3 = undefined
fun (Val f : _ : _ : _ : _) | f == -1 = undefined
fun (Val f : _ : _ : _ : _ : _) | f == -4 = undefined
fun it@[Val f, _] | f == -2 || f == -3 = Left (Fun it)
fun it@[Val (-1), _, _] = Left (Fun it)
fun it@[Val (-4), _, _, _] = Left (Fun it)
fun it@(Val n:_) | n < 0 = Right it
fun (e:it) = Left (foldl (`Bin` Mul) e it)

-- instance Show Encoding where
--     show (Encoding e) = case e of
--                           Left it  -> show it
--                           Right it -> show it

fromInt :: Int -> Encoding
fromInt n = fromInteger (toInteger n)


toC :: [Int] -> C
toC _ = undefined

toC' :: [Encoding] -> C
toC' [Encoding (Left m), Encoding (Left n), Encoding (Left p)] =
    C (fromInteger m') (fromInteger n') (fromInteger p')
    where m' = eval m
          n' = eval n
          p' = eval p

toD :: [Int] -> D
toD _ = undefined

toD' :: [Encoding] -> D
toD' [Encoding (Left q), Encoding (Left r)] =
    D (fromInteger q') (fromInteger r')
    where q' = eval q
          r' = eval r


main :: IO ()
main = do
    x <- pure $ (-1) * 1 * 2 + (-2) * 5 + (-3) * 4 - (-3) * 2 + (-4) * 2 * 3 * 1 : 0 - 1 + 9 : (-3) * 3 : []
    let C a b c = toC x
    y <- pure $ (-1) * (a - 55) * c + (-2) * b - (-4) * (a - 50) * b * 1 : (-4) * b * c * (1 * 1) : []
    let D d e = toD y
    -- x <- pure $ (fromInt (-1)) * (fromInt 1) * (fromInt 2) + (fromInt (-2)) * (fromInt 5) + (fromInt (-3)) * (fromInt 4) - (fromInt (-3)) * (fromInt 2) + (fromInt (-4)) * (fromInt 2) * (fromInt 3) * (fromInt 1) : (fromInt 0) - (fromInt 1) + (fromInt 9) : (fromInt (-3)) * (fromInt 3) : []
    -- let C a b c = toC' x
    -- y <- pure $ (fromInt (-1)) * (fromInt (a - 55)) * (fromInt c) + (fromInt (-2)) * (fromInt b) - (fromInt (-4)) * (fromInt (a - 50)) * (fromInt b) * (fromInt 1) : (fromInt (-4)) * (fromInt b) * (fromInt c) * (fromInt (1) * (fromInt 1)) : []
    -- let D d e = toD' y
    z :: Int <- pure (a + b + d - c * e)
    _u :: () <- print z
    return ()
