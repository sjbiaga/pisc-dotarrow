module Fun ( get, get'
           , ack, ack'
           , fib, fib'
           , fac, fac'
           , sud, sud' ) where

import Control.Monad ((>=>))
import Data.Functor  ((<&>))


data Trampoline a where
    Done :: { value :: a } -> Trampoline a
    Call :: { closure :: () -> Trampoline a } -> Trampoline a
    FlatMap :: { self :: Trampoline a, sequel :: a -> Trampoline b } -> Trampoline b

get :: Trampoline a -> a
get (Done v)                  = v
get (Call c)                  = get (c ())
get (FlatMap (Done v) q)      = get (q v)
get (FlatMap (Call c) q)      = get (c () >>= q)
get (FlatMap (FlatMap s p) q) = get (FlatMap s (p >=> q))

get' :: Trampoline a -> Either (Trampoline a) a
get' (Done v)                  = Right v
get' (Call c)                  = Left (c ())
get' (FlatMap (Done v) q)      = Left (q v)
get' (FlatMap (Call c) q)      = Left (c () >>= q)
get' (FlatMap (FlatMap s p) q) = Left (FlatMap s (p >=> q))

instance Functor Trampoline where
    fmap f s = s >>= pure . f
instance Applicative Trampoline where
    pure                   = Done
    Done f <*> Done v      = Done (f v)
    it@(Done _) <*> Call c = it <*> c ()
    Done f <*> FlatMap s q = FlatMap s (fmap f . q)
    Call f <*> it          = f () <*> it
    FlatMap s q <*> it     = FlatMap s ((<*> it) . q)
instance Monad Trampoline where
    it >>= f = FlatMap it f

instance Show a => Show (Trampoline a)
    where show (Done v) = "Done " ++ show v
          show it@(Call _) = "Call (\\_ -> ...) >> " ++ case get' it of Right r -> show r
                                                                        Left it' -> show it'
          show it@(FlatMap _ _) = "FlatMap (...) (\\a -> ...) >> " ++ case get' it of Right r -> show r
                                                                                      Left it' -> show it'

-- -1 Ackermann
ack :: Integer -> Integer -> Integer
ack m n = get (ack' m n)
ack' :: Integer -> Integer -> Trampoline Integer
ack' m n
  | m == 0    =              Done (n+1)
  | n == 0    =              Call (\_ -> ack' (m-1) 1)
  | otherwise =              Call (\_ -> ack' m (n-1))
            >>= (\p ->       Call (\_ -> ack' (m-1) p)
            <&>        \q -> q)

-- -2 Fibonacci
fib :: Integer -> Integer
fib n = get (fib' n)
fib' :: Integer -> Trampoline Integer
fib' n
  | n < 2     =              Done n
  | otherwise =              Call (\_ -> fib' (n-1))
            >>= (\p ->       Call (\_ -> fib' (n-2))
            <&>        \q -> p+q)

-- -3 factorial
fac :: Integer -> Integer
fac n = get (fac' n)
fac' :: Integer -> Trampoline Integer
fac' n
  | n == 0    =       Done 1
  | otherwise =       Call (\_ -> fac' (n-1))
            <&> \p -> n*p

-- -4 Sudan
sud :: Integer -> Integer -> Integer -> Integer
sud m n p = get (sud' m n p)
sud' :: Integer -> Integer -> Integer -> Trampoline Integer
sud' m n p
  | p == 0    =                     Done (m+n)
  | n == 0    =                     Done m
  | otherwise =                     Call (\_ -> sud' m (n-1) p)
            >>= (\q ->              Call (\_ -> sud' q n 0)
            >>=        (\r ->       Call (\_ -> sud' q r (p-1))
            <&>               \s -> s))
