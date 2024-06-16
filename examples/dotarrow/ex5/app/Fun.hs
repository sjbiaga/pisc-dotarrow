module Fun ( ack, fib, fac, sud ) where

import Control.Monad ((>=>))
import Data.Functor ((<&>))


data Trampoline a where
    Done :: { value :: a } -> Trampoline a
    Call :: { closure :: () -> Trampoline a } -> Trampoline a
    FlatMap :: { self :: Trampoline a, sequel :: a -> Trampoline b } -> Trampoline b

get :: Trampoline a -> a
get (Done v) = v
get (Call c) = get (c ())
get (FlatMap (Done v) q) = get (q v)
get (FlatMap (Call c) q) = get (c () >>= q)
get (FlatMap (FlatMap s p) q) = get (FlatMap s (p >=> q))

instance Functor Trampoline where
    fmap f s = s <&> f
instance Applicative Trampoline where
    pure = Done
    Done f <*> Done v = Done (f v)
    it@(Done _) <*> Call c = it <*> c ()
    Done f <*> FlatMap s q = FlatMap s (fmap f . q)
    Call f <*> it = f () <*> it
    FlatMap s q <*> it = FlatMap s ((<*> it) . q)
instance Monad Trampoline where
    Done v >>= f = f v
    Call c >>= f = c () >>= f
    FlatMap (Done v) q >>= f = q v >>= f
    FlatMap (Call c) q >>= f = (c () >>= q) >>= f
    FlatMap (FlatMap s p) q >>= f = FlatMap s (p >=> q) >>= f

instance Show a => Show (Trampoline a)
    where show = show . get

-- -1 Ackermann
ack :: Integer -> Integer -> Integer
ack m n = get (ack' m n)
ack' :: Integer -> Integer -> Trampoline Integer
ack' m n = do
  if m == 0
  then return (n+1)
  else if n == 0
       then ack' (m-1) 1
  else do
    p <- ack' m (n-1)
    ack' (m-1) p

-- -2 Fibonacci
fib :: Integer -> Integer
fib n = get (fib' n)
fib' :: Integer -> Trampoline Integer
fib' n = do
  if n < 2
  then return n
  else do
    p <- fib' (n-1)
    q <- fib' (n-2)
    return (p+q)

-- -3 factorial
fac :: Integer -> Integer
fac n = get (fac' n)
fac' :: Integer -> Trampoline Integer
fac' n = do
  if n == 0
  then return 1
  else do
    p <- fac' (n-1)
    return (n*p)

-- -4 Sudan
sud :: Integer -> Integer -> Integer -> Integer
sud m n p = get (sud' m n p)
sud' :: Integer -> Integer -> Integer -> Trampoline Integer
sud' m n p = do
  if p == 0
  then return (m+n)
  else if n == 0
       then return m
  else do
    q <- sud' m (n-1) p
    r <- sud' q n 0
    sud' q r (p-1)
