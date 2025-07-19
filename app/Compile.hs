{-# LANGUAGE LambdaCase #-}

module Compile where

import Parse (Token (..))
import Assembler (Asm, movFolding, generateAsm)
import qualified Assembler as Asm (Asm (..))

import GHC.Float (float2Int)
import Data.Bits ((.&.))
import Data.Maybe (fromJust)
import Debug.Trace (trace)

import qualified Data.HashMap.Strict as Map (HashMap, empty, insert, lookup)

import Control.Monad.State (State, get, put, evalState)

type Environment = Map.HashMap String Word

data IR = FrameFunc String [(String, String)] [IR] |
          LeafFunc String [(String, String)] [IR] |
          Variable String Word IR |
          Call String [IR] |
          VarRef String |
          Inline String | Immediate Int deriving (Show)

data LowerIR = StackRef Word | LoadVar String | Var String Word | Label String | Enter Word | Leave | MovReg String String | AsmCall String | AsmInline String deriving (Show)

-- Phase 1: Convert to a more meaningful IR

token2ir :: Token -> Maybe IR
token2ir (Expr ((Ident "define"):xs)) = do
  case xs of
    [Ident label, token] -> do
      ir <- token2ir token 
      Just $ Variable label 0 ir 
    _ -> Nothing
token2ir (Expr ((Ident "defun"):(Ident name):(Expr args):xs)) = do
  ys <- mapM token2ir xs
  let
    paramRegister = ["edi", "esi", "edx", "ecx", "r8d", "r9d"]
    t = (\case
            Ident str -> str
            _ -> "") <$> args :: [String]
    argReg = zip paramRegister t :: [(String, String)]
  Just $ FrameFunc name argReg ys 
token2ir (Expr [Ident "asm", Str asm]) = Just $ Inline asm
token2ir (Num x) = Just . Immediate $ float2Int x
token2ir (Ident x) = Just $ VarRef x
token2ir (Expr ((Ident fname):xs)) = do
  ys <- mapM token2ir xs
  Just $ Call fname ys
token2ir x = trace ("token2ir error in token: " ++ show x) Nothing

-- Phase 2: IR to Lower IR

type ParamReg = (String, String)     
lowerIR :: IR -> [LowerIR]
lowerIR (FrameFunc fname args body) =
  [Label fname, Enter $ next16 reserved] <> bodyir' <> [Leave]
  where bodyir = concatMap lowerIR body :: [LowerIR]
        (reserved, bodyir') = evalState (calcReserved (args, bodyir)) (0, [])

        calcReserved :: ([ParamReg], [LowerIR]) -> State (Word, [LowerIR]) (Word, [LowerIR])
        calcReserved ((reg, param):xs, ir) = do
          (res, ir') <- get
          let offset = res + 4
          put (offset, ir' ++ [MovReg "eax" reg, Var param offset])
          calcReserved (xs, ir)
        calcReserved ([], x:xs) = do
          (res, ir') <- get
          let (offset, x') = case x of
                Var ident _ -> (res + 4, Var ident $ res + 4)
                _ -> (res, x)
          put (offset, ir' ++ [x'])
          calcReserved ([], xs)
        calcReserved ([], []) = get

        next16 b = (0xFFFFFFFFFFFFFFF0 .&. b) + 16
        
lowerIR (Variable name reg ir) =
  lowerIR ir <> [Var name reg]
lowerIR (Call fname args) = buildParams args <> [AsmCall fname]
    where
    paramRegister = ["edi", "esi", "edx", "ecx", "r8d", "r9d"]
    buildParams :: [IR] -> [LowerIR]
    buildParams = setupParams . zip paramRegister
    setupParams :: [(String, IR)] -> [LowerIR] 
    setupParams = foldr (\(reg, ir) acc -> acc <> lowerIR ir <> [MovReg reg "eax"]) []
lowerIR (Immediate v) = [MovReg "eax" $ show v]
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
    v@(Var ident offset) -> put (Map.insert ident offset env, ir ++ [v])
    Leave -> put (Map.empty, ir ++ [Leave])
    x -> put (env, ir ++ [x])
  replaceIdents xs

-- Phase 3: Convert IR to Assembly

lowerIR2asm :: LowerIR -> [Asm]
lowerIR2asm (Var _ offset) = [Asm.Mov ("[rbp-" ++ show offset ++ "]") "eax"]
lowerIR2asm (StackRef offset) = [Asm.Mov "eax" $ "[rbp-" ++ show offset ++ "]"]
lowerIR2asm (Label ident) = [Asm.Label ident]
lowerIR2asm (Enter reserved) = [Asm.Push "rbp", Asm.Mov "rbp" "rsp", Asm.Sub "rsp" $ show reserved]
lowerIR2asm Leave = [Asm.Mov "rsp" "rbp", Asm.Pop "rbp", Asm.Ret]
lowerIR2asm (MovReg to from) = [Asm.Mov to from] 
lowerIR2asm (AsmCall ident) = [Asm.Call ident]
lowerIR2asm (AsmInline asm) = [Asm.Inline asm]
lowerIR2asm x = trace (show x) []

compile :: [Token] -> Maybe String
compile tokens = do
  ir <- mapM token2ir tokens
  let
    lowerir = concatMap lowerIR ir
    lowerir' = evalState (replaceIdents lowerir) (Map.empty, [])
    asm = concatMap lowerIR2asm lowerir' :: [Asm]
  Just $ generateAsm $ movFolding asm []
