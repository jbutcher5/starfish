{-# LANGUAGE LambdaCase #-}

module AST where

import Parse (Token (..))
import Misc (Result (..), Operand (..), systemV, (><))
import GHC.Float (float2Int)
import Data.Maybe (fromJust)

import qualified Data.HashMap.Strict as Map (HashMap, empty, insert, lookup)
import Control.Monad.State (State, get, put, evalState)
type FunctionMap = Map.HashMap String Type 

type TypeSignature = (Type, [Type])

data Type = TPtr Type | TIntegral | TChar

instance Show Type where
  show (TPtr t) = "Ptr [" ++ show t ++ "]"
  show TIntegral = "Int"
  show TChar = "Char"

data AST = ASTExtern [String] |
          ASTFunc String Type [(Operand, String, Type)] [AST] |
          ASTCCall String TypeSignature |
          ASTVar String AST Type |
          ASTCall String [AST] (Result Type) |
          ASTVarRef String |
          ASTInline String |
          ASTSpecialForm [Token] |
          ASTDeref AST Type |
          ASTIntegral Int |
          ASTStr String |
          ASTIf (Maybe Int) AST AST AST
          deriving (Show)

stripStrings :: [Token] -> Result [String]
stripStrings xs = mapM (\case
                        Ident str -> Success str
                        x -> Error $
                          "Could not convert " ++ show x ++ " in " ++ show xs ++ " to a String") xs

toType :: String -> Result Type
toType "Char" = Success TChar 
toType "Int" = Success TIntegral
toType ('*':xs) = Success TPtr <*> toType xs 
toType t = Error $ t ++ " is not a valid type"

token2ast :: Token -> Result AST
token2ast (Expr [Ident "define", Ident t, Ident label, token]) =
  ASTVar label <$> token2ast token <*> toType t
token2ast (Expr [Ident "define", Ident label, token]) = do
  ast <- token2ast token
  Success (ASTVar label ast) <*> typePropagation ast 
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
  ASTFunc name returnType argReg <$> mapM token2ast xs

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
  Success $ ASTFunc name returnType argReg ast 

token2ast (Expr (Ident "defun":xs)) = Error $ "defun must be within the form (defun name Type (args) *body) not " ++ show xs
  
token2ast (Expr [Ident "asm", Str asm]) = Success $ ASTInline asm 
token2ast (Expr form@[Ident "ref", expr]) = Success $ ASTSpecialForm form

token2ast (Expr [Ident "deref", Ident t, expr]) =
  ASTDeref <$> token2ast expr <*> toType t
token2ast (Expr [Ident "deref", expr]) = do
  ast <- token2ast expr
  t <- case typePropagation ast of
    Success (TPtr t) -> Success t
    Success t -> Error "Deref must dereference a pointer"
    e -> e
  Success $ ASTDeref ast t
token2ast (Expr (Ident "deref":xs)) = Error $ "deref must be within the form (deref Type Ptr) not " ++ show xs

token2ast (Expr [Ident "ccall", Ident fname, Ident return, Expr parameters]) = do
  returnType <- toType return
  paramTypes <- mapM toType =<< stripStrings parameters
  Success $ ASTCCall fname (returnType, paramTypes)
  
token2ast (Expr (Ident "extern":externs)) = ASTExtern <$> stripStrings externs
token2ast (Expr [Ident "extern"]) = Error "extern must not be empty"

token2ast (Expr [Ident "if", cond, t, f]) =
  ASTIf Nothing <$> token2ast cond <*> token2ast t <*> token2ast f 

token2ast (Num x) = Success . ASTIntegral $ float2Int x
token2ast (Ident x) = Success $ ASTVarRef x
token2ast (Expr ((Ident fname):xs)) = do
  ast <- mapM token2ast xs
  Success $ ASTCall fname ast defaultError 

  where
    defaultError = Error $ "Can't find a definition for " ++ fname

token2ast (Str s) = Success $ ASTStr s
token2ast expr = Error $ "Unkown expr " ++ show expr

line2ast :: (Int, Token) -> Result AST
line2ast (line, token) =
  case token2ast token of
    s@(Success _) -> s
    Error s -> Error $ "Error on line " ++ show line ++ "\n" ++ s

typePropagation :: AST -> Result Type
typePropagation (ASTIntegral _) = Success TIntegral
typePropagation (ASTStr _) = Success $ TPtr TChar
typePropagation (ASTCall _ _ t) = t
typePropagation (ASTDeref _ t) = Success t
typePropagation x = Error $ "Cannot guess type from " ++ show x

type ASTState = (Int, FunctionMap, Result [AST])

thd :: (a, b, c) -> c
thd (_, _, c) = c

typeCalls :: [AST] -> Result [AST]
typeCalls ast = thd $ typeCalls' ast Map.empty 0 

typeCalls' :: [AST] -> FunctionMap -> Int -> ASTState 
typeCalls' ast env id = evalState (typeCallsS ast) (id, env, Success []) 

concatAST :: Monad m => m [AST] -> m AST -> m [AST]
concatAST ast node = do
  node' <- node
  (><) [node'] <$> ast

evalIfBody :: (Int, FunctionMap) -> AST -> AST -> AST -> (Int, FunctionMap, Result AST)
evalIfBody s cond t f =
   (2 + id'', table'', case (cond', t', f') of
       (Success cond'', Success t'', Success f'') ->
         Success $ ASTIf (Just $ 1 + id'') cond'' t'' f''
       e -> Error "Test")
   
  where
    (id, table, cond') = updateAST s cond
    (id', table', t') = updateAST (id, table) t
    (id'', table'', f') = updateAST (id', table') f
 

updateAST :: (Int, FunctionMap) -> AST -> (Int, FunctionMap, Result AST)
updateAST s@(id, env) = \case
  c@(ASTCCall fname (ret, _)) -> (id, Map.insert fname ret env, Success c)
  (ASTIf Nothing cond t f) -> evalIfBody s cond t f 
  f@(ASTFunc fname ret args body) ->
    case typeCalls' body env id of
      (id, env, Success body) ->
        (id, Map.insert fname ret env, Success $ ASTFunc fname ret args body)
      (id, env, Error e) -> (id, env, Error e)
  (ASTCall fname args d) -> (id, env, Success $ ASTCall fname args maybesig)
      where maybesig = Success . fromJust $ Map.lookup fname env :: Result Type
  x -> (id, env, Success x)

typeCallsS :: [AST] -> State ASTState ASTState 
typeCallsS [] = do get 
typeCallsS (x:xs) = do
  (id, table, ast) <- get
  let (id', table', node) = updateAST (id, table) x
  put (id', table', concatAST ast node)
  typeCallsS xs
