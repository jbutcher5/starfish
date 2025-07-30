{-# LANGUAGE LambdaCase #-}

module AST where

import Parse (Token (..))
import Misc (Result (..), Operand (..), systemV, (><))
import GHC.Float (float2Int)
import Data.Maybe (fromJust)

import Debug.Trace

import qualified Data.HashMap.Strict as Map (HashMap, empty, insert, lookup)
import Control.Monad.State (State, get, put, evalState)
type FunctionMap = Map.HashMap String Type 

type TypeSignature = (Type, [Type])

data Type = Ptr Type | I | C

instance Show Type where
  show (Ptr t) = "Ptr [" ++ show t ++ "]"
  show I = "Int"
  show C = "Char"

data AST = Extern [String] |
          FrameFunc String Type [(Operand, String, Type)] [AST] |
          CCall String TypeSignature |
          Variable String Type Word AST |
          Call String [AST] (Result Type) |
          VarRef String |
          Inline String |
          SpecialForm [Token] |
          Deref AST Type |
          Integral Int |
          StrLiteral String
          deriving (Show)

stripStrings :: [Token] -> Result [String]
stripStrings xs = mapM (\case
                        Ident str -> Success str
                        x -> Error $
                          "Could not convert " ++ show x ++ " in " ++ show xs ++ " to a String") xs

toType :: String -> Result Type
toType "Char" = Success C 
toType "Int" = Success I
toType ('*':xs) = do
  t <- toType xs
  Success $ Ptr t
toType t = Error $ t ++ " is not a valid type"

token2ast :: Token -> Result AST
token2ast (Expr [Ident "define", Ident t, Ident label, token]) = do
  t' <- toType t
  Variable label t' 0 <$> token2ast token
token2ast (Expr [Ident "define", Ident label, token]) = do
  ast <- token2ast token
  t <- typePropagation ast 
  Success $ Variable label t 0 ast 
token2ast (Expr (Ident "define":xs)) = Error $ "define type must be determinable at compile time " ++ show xs

token2ast (Expr ((Ident "defun"):(Ident name):(Ident ret):(Expr args):xs)) = do
  returnType <- toType ret
  shit <- mapM (\case
                      Expr [Ident t, Ident x] -> do
                        t' <- toType t
                        Success (t', x)
                      e -> Error $ show e ++ " does not match pattern (Type, paramName)") args
  let (paramTypes, argNames) = unzip shit :: ([Type], [String])
      argReg = zip3 systemV argNames paramTypes
  FrameFunc name returnType argReg <$> mapM token2ast xs

token2ast (Expr ((Ident "defun"):(Ident name):(Expr args):xs)) = do
  ast <- mapM token2ast xs
  returnType <- typePropagation $ last ast
  shit <- mapM (\case
                      Expr [Ident t, Ident x] -> do
                        t' <- toType t
                        Success (t', x)
                      e -> Error $ show e ++ " does not match pattern (Type, paramName)") args
  let (paramTypes, argNames) = unzip shit :: ([Type], [String])
      argReg = zip3 systemV argNames paramTypes
  Success $ FrameFunc name returnType argReg ast 

token2ast (Expr (Ident "defun":xs)) = Error $ "defun must be within the form (defun name Type (args) *body) not " ++ show xs
  
token2ast (Expr [Ident "asm", Str asm]) = Success $ Inline asm 
token2ast (Expr form@[Ident "ref", expr]) = Success $ SpecialForm form

token2ast (Expr [Ident "deref", Ident t, expr]) =
  Deref <$> token2ast expr <*> toType t
token2ast (Expr [Ident "deref", expr]) = do
  ast <- token2ast expr
  t <- case typePropagation ast of
    Success (Ptr t) -> Success t
    Success t -> Error "Deref must dereference a pointer"
    e -> e
  Success $ Deref ast t
token2ast (Expr (Ident "deref":xs)) = Error $ "deref must be within the form (deref Type Ptr) not " ++ show xs

token2ast (Expr [Ident "ccall", Ident fname, Ident return, Expr parameters]) = do
  returnType <- toType return
  paramTypes <- mapM toType =<< stripStrings parameters
  Success $ CCall fname (returnType, paramTypes)
  
token2ast (Expr (Ident "extern":externs)) = Extern <$> stripStrings externs
token2ast (Expr [Ident "extern"]) = Error "extern must not be empty"

token2ast (Num x) = Success . Integral $ float2Int x
token2ast (Ident x) = Success $ VarRef x
token2ast (Expr ((Ident fname):xs)) = do
  ast <- mapM token2ast xs
  Success . Call fname ast $ Error $ "Can't find a definition for " ++ fname 
token2ast (Str s) = Success $ StrLiteral s

tokn2ast expr = Error $ "Unkown expr " ++ show expr

line2ast :: (Int, Token) -> Result AST
line2ast (line, token) =
  case token2ast token of
    s@(Success _) -> s
    Error s -> Error $ "Error on line " ++ show line ++ "\n" ++ s

typePropagation :: AST -> Result Type
typePropagation (Integral _) = Success I
typePropagation (StrLiteral _) = Success $ Ptr C
typePropagation (Call _ _ t) = t
typePropagation (Deref _ t) = Success t
typePropagation x = Error $ "Cannot guess type from " ++ show x

-- TODO: Implement Type Checking but AST nor IR seem like the right place

-- generateTypeTable :: [AST] -> FunctionMap
-- generateTypeTable ast = evalState (generateTypeTableS ast) Map.empty

-- generateTypeTableS :: [AST] -> State FunctionMap FunctionMap
-- generateTypeTableS [] = get
-- generateTypeTableS (x:xs) = do
--   table <- get
--   case x of
--     FrameFunc fname (Success ret) _ _ -> put $ Map.insert fname ret table 
--     Variable ident (Success t) _ _ -> put $ Map.insert ident t table
--   generateTypeTableS xs

-- getIdent :: FunctionMap -> String -> Result Type
-- getIdent table ident = case Map.lookup ident table of
--   Just t -> Success t
--   Nothing -> Error $ "Unable to determine type of " ++ ident

-- updateAST :: FunctionMap -> AST -> (AST, Bool)
-- updateAST table (VarRef ident (Error _)) = (VarRef ident $ getIdent table ident, True)
-- updateAST table (Call fname args (Error _)) = (Call fname args $ getIdent table fname, True) 
-- updateAST table (Deref ast (Error _)) = (Deref ast' newT, True)  
--   where (ast', _) = unzip $ updateAST table ast
--         t = typePropagation ast'
--         newT = case t of
--           Success (Ptr t') -> Success t'
--           t' -> Error $ "Cannot dereference type " ++ show t'
-- updateAST _ x = (x, False)

-- isTyped :: AST -> Bool
-- isTyped (VarRef _ t) = False
-- isTyped (Call _ _ t) = False
-- isTyped (Deref _ t) = False
-- isTyped (Variable _ (Error _) _ _) = False
-- isTyped x = True

-- solveTypes' :: FunctionMap -> [AST] -> [AST]
-- solveTypes' table ast =
--   if and updated then solveTypes' (generateTypeTable ast') ast' else ast'

--   where (ast', updated) = unzip $ updateAST table <$> ast
-- solveTypes :: [AST] -> [AST]
-- solveTypes ast = trace (show $ fmap isTyped ast) $
--   solveTypes' table ast

--   where table = generateTypeTable ast 

typeCalls :: [AST] -> Result [AST]
typeCalls ast = typeCalls' ast Map.empty 

typeCalls' :: [AST] -> FunctionMap -> Result [AST]
typeCalls' ast env = evalState (typeCallsS ast) (env, Success []) 

typeCallsS :: [AST] -> State (FunctionMap, Result [AST]) (Result [AST]) 
typeCallsS [] = do
  (_, ast) <- get
  return ast
typeCallsS (x:xs) = do
  (env, ast) <- get
  case x of
    c@(CCall fname (ret, _)) -> put (Map.insert fname ret env, (:) c <$> ast)
    f@(FrameFunc fname ret args body) -> do
      let s = typeCallsS body :: State (FunctionMap, Result [AST]) (Result [AST])
          l = (\x -> [FrameFunc fname ret args x]) <$> evalState s (env, Success []) :: Result [AST]
      case typeCalls' body env of
        Success body -> put (Map.insert fname ret env, (><) [FrameFunc fname ret args body] <$> ast)
        e -> put (env, e)
    Call fname args d -> put (env, (><) [Call fname args maybesig] <$> ast)
      where maybesig = Success . fromJust $ Map.lookup fname env :: Result Type
    x -> put (env, (><) [x] <$> ast)
  typeCallsS xs

-- updateAST :: AST -> FunctionMap -> AST
