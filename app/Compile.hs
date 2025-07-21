{-# LANGUAGE BlockArguments #-}

module Compile where

import Parse (Token (..))
import AST (AST (..), token2ast)
import Misc (Result (..), Operand (..), systemV)

import Data.Bits ((.&.))
import Data.Maybe (fromJust)

import qualified Data.HashMap.Strict as Map (HashMap, empty, insert, lookup)

import Control.Monad.State (State, get, put, evalState)

type Environment = Map.HashMap String (Word, Word)

data IR = LoadMemory Word Word | GetRef String | LoadRef Word Word | LoadVar String | Var String Word Word | Enter String Word | Leave | MovReg Operand Operand | AsmCall String | AsmInline String deriving (Show)


(><) :: Semigroup a => a -> a -> a
a >< b = b <> a

-- Phase 2: AST to IR

type ParamReg = (Operand, String)     
ast2ir :: AST -> Result [IR]
ast2ir (FrameFunc fname args body) = do
  bodyir <- concat <$> mapM ast2ir body
  let (reserved, bodyir') = evalState (calcReserved (args, bodyir)) (0, [])

      -- Calculate how much stack space is needed for each function
      -- Also, update variable offsets
      calcReserved :: ([ParamReg], [IR]) -> State (Word, [IR]) (Word, [IR])
      calcReserved ((reg, param):xs, ir) = do
        (res, ir') <- get
        let offset = res + 4 -- TODO: Remove and size parameters sometimes
        put (offset, ir' ++ [MovReg Reg{suffix="ax", size=4} reg, Var param 4 offset])
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
        
ast2ir (Variable name size reg ast) = (><) [Var name size reg] <$> ast2ir ast
ast2ir (Call fname args) = do
  let  buildParams :: [AST] -> Result [IR]
       buildParams = setupParams . zip systemV
       setupParams :: [(Operand, AST)] -> Result [IR] 
       setupParams = foldr (\(reg, ir) acc -> do
                               a <- acc
                               x <- ast2ir ir
                               Success $ a <> x <> [MovReg reg Reg{suffix="ax", size=4}]) (Success [])

  x <- buildParams args
  Success $ x <> [AsmCall fname]
ast2ir (Integral v) = Success [MovReg Reg {suffix="ax", size=8} . Immediate $ show v]
ast2ir (VarRef ident) = Success [LoadVar ident]
ast2ir (SpecialForm [Ident "ref", Ident ident]) = Success [GetRef ident]
ast2ir (Deref ast) = do
  ir <- ast2ir ast
  Success $ ir <> [MovReg Reg {suffix="ax", size=8} $ Referenced Reg {suffix="ax", size=8}]
ast2ir (Inline asm) = Success [AsmInline asm] 
ast2ir x = Error $ "ast2ir error in token: " ++ show x

-- Phase 2.5: Replace variables

replaceIdents :: [IR] -> State (Environment, [IR]) [IR]
replaceIdents [] = do
  (_, ir) <- get
  return ir
replaceIdents (x:xs) = do
  (env, ir) <- get
  case x of
    LoadVar ident -> put (env, ir ++ [LoadMemory size offset])
      where (offset, size) = fromJust $ Map.lookup ident env
    GetRef ident -> put (env, ir ++ [LoadRef offset size])
      where (offset, size) = fromJust $ Map.lookup ident env
    v@(Var ident size offset) -> put (Map.insert ident (offset, size) env, ir ++ [v])
    Leave -> put (Map.empty, ir ++ [Leave])
    x -> put (env, ir ++ [x])
  replaceIdents xs

