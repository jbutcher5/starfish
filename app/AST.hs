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
          Deref AST |
          Integral Int |
          StrLiteral String
          deriving (Show)

stripStrings :: [Token] -> Result [String]
stripStrings xs = mapM (\case
                        Ident str -> Success str
                        x -> Error $
                          "Could not convert " ++ show x ++ " in " ++ show xs ++ " to a String") xs

typeSize :: String -> Result Word
typeSize "Int" = Success 4
typeSize "Ptr" = Success 8
typeSize t = Error $ t ++ " is not a valid type"

token2ast :: Token -> Result AST
token2ast (Expr ((Ident "define"):(Ident t):xs)) = do
  size <- typeSize t
  case xs of
    [Ident label, token] -> Variable label size 0 <$> token2ast token
    _ -> Error $ "Illegal Define: " ++ show xs
token2ast (Expr ((Ident "defun"):(Ident name):(Expr args):xs)) = do
  strings <- stripStrings args
  let argReg = zip systemV strings
  FrameFunc name argReg <$> mapM token2ast xs 
token2ast (Expr [Ident "asm", Str asm]) = Success $ Inline asm 
token2ast (Expr form@[Ident "ref", expr]) = Success $ SpecialForm form
token2ast (Expr [Ident "deref", expr]) = Deref <$> token2ast expr 
token2ast (Expr (Ident "extern":externs)) = Extern <$> stripStrings externs
token2ast (Num x) = Success . Integral $ float2Int x
token2ast (Ident x) = Success $ VarRef x
token2ast (Expr ((Ident fname):xs)) = Call fname <$> mapM token2ast xs
token2ast (Str s) = Success $ StrLiteral s
