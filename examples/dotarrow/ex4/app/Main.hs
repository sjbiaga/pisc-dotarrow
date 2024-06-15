module Main where

import Prelude hiding (exp)

import System.Exit (exitFailure)     --  dotarrow
import System.IO (hPutStrLn, stderr) --  dotarrow
import Inp_gUgVwYdD8r                --  dotarrow
import Out_gUgVwYdD8r                --  dotarrow
import Data.List (intercalate)       --  dotarrow


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
          | Bin { lhs :: Expr, op :: Op, rhs :: Expr }
          | Neg { exp :: Expr }

-- instance Show Expr where
--     show (Val n) = show n
--     show (Neg (Val 0)) = "0"
--     show (Neg (Val n)) | n > 0 = "-" ++ show n
--     show (Neg e) = "-(" ++ show e ++ ")"
--     show (Bin l o r) = l' ++ " " ++ show o ++ " " ++ r'
--         where l' = case l of (Val n) -> show n
--                              _       -> "(" ++ show l ++ ")"
--               r' = case r of (Val n) -> show n
--                              _       -> "(" ++ show r ++ ")"

eval :: Expr -> Integer
eval (Val n) = toInteger n
eval (Bin l o r) = case o of
                     Add -> l' + r'
                     Sub -> l' - r'
                     Mul -> l' * r'
    where l' = eval l
          r' = eval r
eval (Neg e) = 0 - eval e

newtype Encoding = Encoding Expr

instance Num Encoding where
    fromInteger n = Encoding (Val n)
    (Encoding lhs) - (Encoding rhs) = Encoding (Bin lhs Sub rhs)
    negate (Encoding lhs) = Encoding (Neg lhs)
    (Encoding lhs) + (Encoding rhs) = Encoding (Bin lhs Add rhs)
    (Encoding lhs) * (Encoding rhs) = Encoding (Bin lhs Mul rhs)
    abs (Encoding lhs) = Encoding (Val (abs (eval lhs)))
    signum (Encoding lhs) = Encoding (Val (signum (eval lhs)))

-- instance Show Encoding where
--     show (Encoding e) = show e

fromInt :: Int -> Encoding
fromInt n = fromInteger (toInteger n)


toC :: [Int] -> C
toC _ = undefined

toC' :: [Encoding] -> C
toC' [Encoding m, Encoding n, Encoding p] = C (fromInteger m') (fromInteger n') (fromInteger p')
    where m' = eval m
          n' = eval n
          p' = eval p

toD :: [Int] -> D
toD _ = undefined

toD' :: [Encoding] -> D
toD' [Encoding q, Encoding r] = D (fromInteger q') (fromInteger r')
    where q' = eval q
          r' = eval r


main :: IO ()
main = do
    x <- pure $ -(1 + 4 - 1) : 2 + 5 - 1 : 3 + 6 - 1 : []
    let C a b c = toC x
    y <- pure $ a + c : b + 7 : []
    let D d e = toD y
    z :: Int <- pure (a + b + d - c * e)
    _u :: () <- print z
    return ()
