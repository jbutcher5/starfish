module Main where

import Parse (Token (..), value, values)
import Eval (EnvStack, eval)
import Compile (compile)

import Data.HashMap.Strict (empty) 
import Text.Parsec (parse)
import System.IO
import System.Environment (getArgs)

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

evalLines :: String -> IO () 
evalLines content = do
  case parse values "" content of
    Left err -> print err
    Right xs -> evalValues xs $ pure . Right $ (Nil, [empty])
  where
    evalValues :: [Token] -> IO (Either String (Token, EnvStack)) -> IO ()
    evalValues [] acc = acc >> pure () 
    evalValues (x:xs) acc = do
      acc' <- acc
      case acc' of
        Left err -> putStr err
        Right (_, env) -> evalValues xs $ eval env x

compileLines :: String -> IO ()
compileLines content = do
  case parse values "" content of
    Left err -> print err
    Right xs -> case compile xs of
      Just program -> writeFile "comp.asm" program
      Nothing -> print "idfk" 

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["i", path] -> do
      contents <- readFile path
      evalLines contents
    ["c", path] -> do
      contents <- readFile path
      compileLines contents
    [] -> repl [empty]
    _ -> putStr "Could not resolve query"
