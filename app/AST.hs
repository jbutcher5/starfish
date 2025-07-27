{-# LANGUAGE LambdaCase #-}

module AST where

import Parse (Token (..))
import Misc (Result (..), Operand (..), systemV, (><))
import GHC.Float (float2Int)
import Data.Maybe (fromJust)

import qualified Data.HashMap.Strict as Map (HashMap, empty, insert, lookup)
import Control.Monad.State (State, get, put, evalState)
type FunctionMap = Map.HashMap String TypeSignature 

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
  size <- toType t
  Variable label size 0 <$> token2ast token
token2ast (Expr [Ident "define", Ident label, token]) = do
  ast <- token2ast token
  t <- typePropagation ast 
  Success $ Variable label t 0 ast 
token2ast (Expr (Ident "define":xs)) = Error $ "define must be within the form (define Type name expr) not " ++ show xs

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

token2ast (Expr [Ident "deref", Ident t, expr]) = Deref <$> token2ast expr <*> toType t
token2ast (Expr [Ident "deref", expr]) = do
  ast <- token2ast expr
  t <- typePropagation ast
  case t of
    Ptr t' -> Success $ Deref ast t'
    t' -> Error $ "Cannot dereference type " ++ show t' 
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
typePropagation (VarRef ident) = Error $ "Unkown type for identifier " ++ ident
typePropagation (Deref _ t) = Success t
typePropagation x = Error $ "Cannot guess type from " ++ show x

-- TODO: Implement Type Checking but AST nor IR seem like the right place

typeCalls :: [AST] -> State (FunctionMap, Result [AST]) (Result [AST]) 
typeCalls [] = do
  (_, ast) <- get
  return ast
typeCalls (x:xs) = do
  (env, ast) <- get
  case x of
    c@(CCall fname sig) -> put (Map.insert fname sig env, (:) c <$> ast)
    f@(FrameFunc fname ret args body) -> do
      let s = typeCalls body :: State (FunctionMap, Result [AST]) (Result [AST])
          l = (\x -> [FrameFunc fname ret args x]) <$> evalState s (env, Success []) :: Result [AST]
      case l of
        Success newBody -> put (Map.insert fname (ret, []) env, (><) newBody <$> ast)
        e -> put (env, e)
    Call fname args d -> put (env, (><) [Call fname args maybesig] <$> ast)
      where maybesig = Success . fst . fromJust $ Map.lookup fname env :: Result Type
    x -> put (env, (><) [x] <$> ast)
  typeCalls xs
