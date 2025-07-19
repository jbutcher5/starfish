module Main where

import Parse (Token (..), value, values)
import Compile (compile)

import Data.HashMap.Strict (empty) 
import Text.Parsec (parse)
import System.IO
import System.Environment (getArgs)

compileLines :: String -> String -> IO ()
compileLines content output = do
  case parse values "" content of
    Left err -> print err
    Right xs -> case compile xs of
      Just program -> writeFile output program
      Nothing -> print "idfk" 

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["c", path, output] -> do
      contents <- readFile path
      compileLines contents output
    ["c", path] -> do
      contents <- readFile path
      compileLines contents "out.asm"
    _ -> putStr "Could not resolve query"
