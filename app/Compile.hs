module Compile where

import Parse (Token (..))
import GHC.Float (float2Int)
import Data.Bits ((.&.))
import Data.Maybe (fromJust)
import Debug.Trace (trace)

import qualified Data.HashMap.Strict as Map (HashMap, empty, insert, lookup)

import Control.Monad.State (State, get, put, evalState)

type Environment = Map.HashMap String Word

data IR = FrameFunc String Word [IR] |
          LeafFunc String [IR] |
          Variable String Word Word IR |
          Call String [IR] |
          VarRef String |
          Inline String | Immediate Int deriving (Show)

data LowerIR = StackRef Word | LoadVar String | Var String Word Word | Label String | Enter Word | Leave | Mov String String | AsmCall String | AsmInline String deriving (Show)

-- Phase 1: Convert to a more meaningful IR

token2ir :: Token -> Maybe IR
token2ir (Expr ((Ident "define"):xs)) =
  case xs of
    [Ident label, Num x] -> Just . Variable label 4 0 . Immediate $ float2Int x 
    _ -> Nothing
token2ir (Expr ((Ident "defun"):(Ident name):xs)) = do
  ys <- mapM token2ir xs
  let (ir, space) = foldr (\x (acc, rbp) -> case x of
                  Variable ident bytes _ dat ->
                    (Variable ident bytes (bytes + rbp) dat:acc, rbp + bytes)
                  _ -> (x:acc, rbp)) ([], 0) ys :: ([IR], Word)
  Just $ FrameFunc name ((0xFFFFFFFFFFFFFFF0 .&. space) + 16) ir 
token2ir (Expr [Ident "asm", Str asm]) = Just $ Inline asm
token2ir (Num x) = Just . Immediate $ float2Int x
token2ir (Ident x) = Just $ VarRef x
token2ir (Expr ((Ident fname):xs)) = do
  ys <- mapM token2ir xs
  Just $ Call fname ys
token2ir x = trace ("token2ir error in token: " ++ show x) Nothing

-- Phase 2: IR to Lower IR

lowerIR :: IR -> [LowerIR]
lowerIR (FrameFunc fname reserved body) =
  [Label fname, Enter reserved] <> bodyir <> [Leave]
  where bodyir = concatMap lowerIR body :: [LowerIR]
lowerIR (Variable name size offset ir) =
  lowerIR ir <> [Var name size offset]
lowerIR (Call fname args) = buildParams args <> [AsmCall fname]
    where
    paramRegister = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
    buildParams :: [IR] -> [LowerIR]
    buildParams = setupParams . zip paramRegister

    setupParams :: [(String, IR)] -> [LowerIR] 
    setupParams = foldr (\(reg, ir) acc -> acc <> lowerIR ir <> [Mov reg "rax"]) []
lowerIR (Immediate v) = [Mov "rax" $ show v]
lowerIR (VarRef ident) = [LoadVar ident]
lowerIR (Inline asm) = [AsmInline asm] 
lowerIR x = trace ("lowerIR error in token: " ++ show x) []

-- Phase 2.5: Replace variables

replaceIdents :: [LowerIR] -> State (Environment, [LowerIR]) [LowerIR]
replaceIdents [] = do
  (_, ir) <- get
  return ir
replaceIdents (x:xs) = do
  (env, ir) <- get
  case x of
    LoadVar ident -> put (env, ir ++ [StackRef . fromJust $ Map.lookup ident env])
    v@(Var ident size offset) -> put (Map.insert ident offset env, ir ++ [v])
    x -> put (env, ir ++ [x])
  replaceIdents xs

-- Phase 3: Convert IR to Assembly

lowerIR2asm :: LowerIR -> String
lowerIR2asm (Var _ size offset) = "\nmov [rbp-" ++ show offset ++ "], rax"
lowerIR2asm (StackRef offset) = "\nmov rax, [rbp-" ++ show offset ++ "]"
lowerIR2asm (Label ident) = "\n" ++ ident ++ ":"
lowerIR2asm (Enter reserved) = "\npush rbp\nmov rbp, rsp\nsub rsp, " ++ show reserved
lowerIR2asm Leave = "\nleave"
lowerIR2asm (Mov to from) = "\nmov " ++ to ++ ", " ++ from
lowerIR2asm (AsmCall ident) = "\ncall " ++ ident
lowerIR2asm (AsmInline asm) = "\n" ++ asm
lowerIR2asm x = trace (show x) ""

compile :: [Token] -> Maybe String
compile tokens = do
  ir <- mapM token2ir tokens
  let
    lowerir = concatMap lowerIR ir
    lowerir' = evalState (replaceIdents lowerir) (Map.empty, [])
    asm = concatMap lowerIR2asm lowerir' 
  Just $ "global _start\nsection .text" <> asm
