module Misc where

data Result a = Success a | Error String deriving Show

instance Functor Result where
  fmap f (Success a) = Success (f a)
  fmap _ (Error e) = Error e

instance Applicative Result where
  (Success f) <*> m = fmap f m
  (Error e1) <*> (Error e2) = Error $ e1 ++ "\n" ++ e2
  (Error e) <*> _ = Error e
  pure = Success

instance Monad Result where
  (Success a) >>= f = f a
  (Error e) >>= _ = Error e
