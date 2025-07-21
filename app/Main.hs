module Main where

import Parse (Token (..), value, values)
import AST (token2ast)
import Compile (ast2ir, replaceIdents, IR)
import Assembler (ir2asm, generateAsm, Asm)
import Misc (Result (..))

import qualified Data.HashMap.Strict as Map (empty)
import Control.Monad.State (State, evalState)
import Data.HashMap.Strict (empty) 
import Text.Parsec (parse)
import System.IO
import System.Environment (getArgs)

execStaticAnalysis :: [Token] -> Result [IR]
execStaticAnalysis tokens = do
  ast <- mapM token2ast tokens
  ir <- concat <$> mapM ast2ir ast
  Success $ evalState (replaceIdents ir) (Map.empty, [])

compile :: [Token] -> Result String
compile tokens = do
  ir <- execStaticAnalysis tokens
  let asm = concatMap ir2asm ir :: [Asm]
  Success $ generateAsm asm

compileLines :: String -> String -> IO ()
compileLines content output = do
  case parse values "" content of
    Left err -> print err
    Right xs -> case compile xs of
      Success program -> writeFile output program
      Error e -> print e 

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
