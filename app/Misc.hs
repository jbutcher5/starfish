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

data Operand = StackPointer{offset :: Word, size :: Word} |
               Reg{suffix :: String, size :: Word} |
               Referenced Operand |
               GeneralPurpose String |
               Specific String |
               Immediate String deriving Eq 

instance Show Operand where
  show StackPointer {offset=o, size=s} =
    (case s of
       8 -> "QWORD"
       _ -> "DWORD") ++ " [rbp-" ++ show o ++ "]"
  show Reg {suffix=suf, size=s} = case s of
    8 -> 'r' : suf
    _ -> 'e' : suf
  show (Immediate s) = s
  show (Specific s) = s
  show (GeneralPurpose s) = s
  show (Referenced o) = "[" ++ show o ++ "]"

systemV = [
  Reg {suffix="di", size=4}, Reg {suffix="si", size=4},
  Reg {suffix="dx", size=4}, Reg {suffix="cx", size=4},
  GeneralPurpose "r8d", GeneralPurpose "r9d"]

(><) :: Semigroup a => a -> a -> a
a >< b = b <> a
