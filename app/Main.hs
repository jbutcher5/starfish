module Main where

import Parse (Token (..), value, values)
import AST (line2ast, typeCalls)
import Compile (ast2ir, replaceIdents, IR)
import Assembler (ir2asm, generateAsm, generateData, Asm)
import Misc (Result (..))

import qualified Data.HashMap.Strict as Map (empty)
import Control.Monad.State (State, evalState)
import Data.HashMap.Strict (empty) 
import Text.Parsec (parse)
import System.IO
import System.Environment (getArgs)

execStaticAnalysis :: [(Int, Token)] -> Result ([String], [IR])
execStaticAnalysis tokens = do
  ast <- mapM line2ast tokens >>= typeCalls
  ir <- concat <$> mapM ast2ir ast 
  Success $ evalState (replaceIdents ir) (Map.empty, [], [])

compile :: [(Int, Token)] -> Result String
compile tokens = do
  (stringBank, ir) <- execStaticAnalysis tokens
  let asm = concatMap ir2asm ir :: [Asm]
  Success $ generateAsm asm ++ "\n\n" ++ generateData stringBank

compileLines :: String -> String -> IO ()
compileLines content output = do
  case parse values "" content of
    Left err -> print err
    Right xs -> case compile xs of
      Success program -> writeFile output program
      Error e -> putStrLn e 

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
