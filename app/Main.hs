module Main where

import AST (line2ast, updateProgram)
import Assembler (Asm, generateAsm, generateData, ir2asm)
import Compile (IR, addBranchIds, ast2ir, replaceIdents)
import Control.Monad.State (State, evalState)
import Data.HashMap.Strict (empty)
import qualified Data.HashMap.Strict as Map (empty)
import Misc (Result (..))
import Parse (Token (..), value, values)
import System.Environment (getArgs)
import System.IO
import Text.Parsec (parse)

execStaticAnalysis :: [(Int, Token)] -> Result ([String], [IR])
execStaticAnalysis tokens = do
  ast <- mapM line2ast tokens
  ir <- concat <$> mapM ast2ir (updateProgram ast)
  Success $ evalState (replaceIdents $ addBranchIds ir) (Map.empty, [], [])

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
