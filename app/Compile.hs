{-# LANGUAGE BlockArguments #-}

module Compile where

import Parse (Token (..))
import AST (AST (..), TypeSignature, token2ast, Type (..))
import Misc (Result (..), Operand (..), systemV, (><))

import Data.Bits ((.&.))
import Data.Maybe (fromJust)

import qualified Data.HashMap.Strict as Map (HashMap, empty, insert, lookup)
import Control.Monad.State (State, get, put, evalState)
type Environment = Map.HashMap String (Word, Word)

data IR = LoadMemory Word Word | StringLiteral String |
          LoadRef String | GetVarRef String |
          LoadVarRef Word Word | LoadVar String |
          Var String Word Word | Enter String Word |
          Leave | MovReg Operand Operand |
          AsmCall String | AsmInline String deriving (Show)

typeSize :: Type -> Word
typeSize TChar = 1
typeSize TIntegral = 4
typeSize (TPtr _) = 8

-- Phase 2: AST to IR

type ParamReg = (Operand, String, Type)     
ast2ir :: AST -> Result [IR]
ast2ir (ASTFunc fname _ args body) = do
  bodyir <- concat <$> mapM ast2ir body
  let (reserved, bodyir') = evalState (calcReserved (args, bodyir)) (0, [])

      -- Calculate how much stack space is needed for each function
      -- Also, update variable offsets
      calcReserved :: ([ParamReg], [IR]) -> State (Word, [IR]) (Word, [IR])
      calcReserved ((reg, param, t):xs, ir) = do
        (res, ir') <- get
        let size = typeSize t
            offset = res + size
        put (offset, ir' ++ [MovReg Reg{suffix="ax", size=size} reg, Var param size offset])
        calcReserved (xs, ir)
      calcReserved ([], x:xs) = do
        (res, ir') <- get
        let (offset, x') = case x of
              Var ident size _ -> (res + size, Var ident size $ res + size)
              _ -> (res, x)
        put (offset, ir' ++ [x'])
        calcReserved ([], xs)
      calcReserved ([], []) = get

      next16 b = (0xFFFFFFFFFFFFFFF0 .&. b) + 16

  Success $ [Enter fname $ next16 reserved] <> bodyir' <> [Leave]

ast2ir (ASTCCall fname _) = Success [AsmInline $ "extern " ++ fname] 
ast2ir (ASTVar name ast t) = do
  (><) [Var name (typeSize t) 0] <$> ast2ir ast
ast2ir (ASTCall fname args t) = do
  t <- t
  let  buildParams :: [AST] -> Result [IR]
       buildParams = setupParams . zip systemV
       setupParams :: [(Operand, AST)] -> Result [IR] 
       setupParams = foldr (\(reg, ir) acc -> do
                               a <- acc
                               x <- ast2ir ir
                               Success $ a <> x <> [MovReg reg Reg{suffix="ax", size=typeSize t}]) (Success [])

  x <- buildParams args
  Success $ x <> [AsmCall fname]
ast2ir (ASTIntegral v) = Success [MovReg Reg {suffix="ax", size=8} . Immediate $ show v]
ast2ir (ASTVarRef ident) = Success [LoadVar ident]
ast2ir (ASTSpecialForm [Ident "ref", Ident ident]) = Success [GetVarRef ident]
ast2ir (ASTDeref ast t) = do
  ir <- ast2ir ast
  Success $ ir <> [MovReg Reg {suffix="ax", size=typeSize t} $ Referenced Reg {suffix="ax", size=8}]
ast2ir (ASTInline asm) = Success [AsmInline asm]
ast2ir (ASTStr s) = Success [StringLiteral s]
ast2ir x = Error $ "ast2ir error in token: " ++ show x

-- Phase 2.5: Replace variables
-- TODO: Break this out into it's own distict step
-- Maybe have different IR.
-- See if it can be moved between the token2ast and ast2ir step

replaceIdents :: [IR] -> State (Environment, [String], [IR]) ([String], [IR])
replaceIdents [] = do
  (_, stringBank, ir) <- get
  return (stringBank, ir)
replaceIdents (x:xs) = do
  (env, stringBank, ir) <- get
  case x of
    LoadVar ident -> put (env, stringBank, ir ++ [LoadMemory size offset])
      where (offset, size) = fromJust $ Map.lookup ident env
    GetVarRef ident -> put (env, stringBank, ir ++ [LoadVarRef offset size])
      where (offset, size) = fromJust $ Map.lookup ident env
    v@(Var ident size offset) -> put (Map.insert ident (offset, size) env, stringBank, ir ++ [v])
    StringLiteral s -> put (env, s:stringBank, ir ++ [LoadRef $ "LC" ++ show (length stringBank)])
    Leave -> put (Map.empty, stringBank, ir ++ [Leave])
    x -> put (env, stringBank, ir ++ [x])
  replaceIdents xs
