{-# LANGUAGE LambdaCase #-}

module AST where

import Parse (Token (..))
import Misc (Result (..), Operand (..), systemV)
import GHC.Float (float2Int)

data AST = Extern [String] |
          FrameFunc String [(Operand, String)] [AST] |
          LeafFunc String [(Operand, String)] [AST] |
          Variable String Word Word AST |
          Call String [AST] |
          VarRef String |
          Inline String |
          SpecialForm [Token] |
          Deref AST Word |
          Integral Int |
          StrLiteral String
          deriving (Show)

stripStrings :: [Token] -> Result [String]
stripStrings xs = mapM (\case
                        Ident str -> Success str
                        x -> Error $
                          "Could not convert " ++ show x ++ " in " ++ show xs ++ " to a String") xs

typeSize :: String -> Result Word
typeSize "Char" = Success 1
typeSize "Int" = Success 4
typeSize "Ptr" = Success 8
typeSize t = Error $ t ++ " is not a valid type"

token2ast :: Token -> Result AST
token2ast (Expr [Ident "define", Ident t, Ident label, token]) = do
  size <- typeSize t
  Variable label size 0 <$> token2ast token
token2ast (Expr (Ident "define":xs)) = Error $ "define must be within the form (define Type name expr) not " ++ show xs

token2ast (Expr ((Ident "defun"):(Ident name):(Expr args):xs)) = do
  strings <- stripStrings args
  let argReg = zip systemV strings
  FrameFunc name argReg <$> mapM token2ast xs
token2ast (Expr (Ident "defun":xs)) = Error $ "defun must be within the form (defun name (args) *body) not " ++ show xs
  
token2ast (Expr [Ident "asm", Str asm]) = Success $ Inline asm 
token2ast (Expr form@[Ident "ref", expr]) = Success $ SpecialForm form

token2ast (Expr [Ident "deref", Ident sizeStr, expr]) = Deref <$> token2ast expr <*> typeSize sizeStr
token2ast (Expr (Ident "deref":xs)) = Error $ "deref must be within the form (deref Type Ptr) not " ++ show xs

token2ast (Expr (Ident "extern":externs)) = Extern <$> stripStrings externs
token2ast (Expr [Ident "extern"]) = Error "extern must not be empty"

token2ast (Num x) = Success . Integral $ float2Int x
token2ast (Ident x) = Success $ VarRef x
token2ast (Expr ((Ident fname):xs)) = Call fname <$> mapM token2ast xs
token2ast (Str s) = Success $ StrLiteral s

tokn2ast expr = Error $ "Unkown expr " ++ show expr

line2ast :: (Int, Token) -> Result AST
line2ast (line, token) =
  case token2ast token of
    s@(Success _) -> s
    Error s -> Error $ "Error on line " ++ show line ++ "\n" ++ s
