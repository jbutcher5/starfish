module Main where

import Parse (value)
import Eval (EnvStack, eval)

import Data.HashMap.Strict (empty) 
import Text.Parsec (parse)
import System.IO

repl :: EnvStack -> IO ()
repl env = do
  putStr "> "
  hFlush stdout
  s <- getLine
  case parse value "" s of
    Left err -> print err >> repl env
    Right x -> do
      result <- eval env x
      case result of
        Right (x, new_env) -> print x >> repl new_env
        Left err -> print err >> repl env

main :: IO ()
main = repl [empty]
