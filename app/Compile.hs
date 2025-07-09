module Compile where

import Parse (Token (..))
import GHC.Float (float2Int)

data IR = Func String [IR] | Variable String Int Int | Call String

token2ir :: Token -> Maybe IR
token2ir (Expr ((Ident "define"):xs)) =
  case xs of
    [Ident label, Num x] -> Just $ Variable label 4 $ float2Int x 
    _ -> Nothing
token2ir (Expr ((Ident "defun"):(Ident name):xs)) = do
  ys <- mapM token2ir xs
  Just $ Func name ys 
token2ir _ = Nothing

instr2asm :: IR -> Int -> (String, Int)
instr2asm (Variable ident b x) rbp
  = ("mov DWORD [rbp-" <> show (rbp + b) <> "], " <> show x, b)
instr2asm (Func name body) rbp
  = (name <> ":\npush rbp\nmov rbp, rsp" <> ir2asm body <> "\npop rbp\nret", 0)
instr2asm _ _ = ("", 0)

accumulate :: IR -> (String, Int) -> (String, Int)
accumulate ir (program, rbp) = (program <> "\n" <> asm, rbp + offset)
  where (asm, offset) = instr2asm ir rbp 

ir2asm :: [IR] -> String
ir2asm ir = program
  where (program, _) = foldr accumulate ("", 0) ir

compile :: [Token] -> Maybe String
compile tokens = do
  ir <- mapM token2ir tokens
  Just $ (<>) "global _start\nsection .text\n" $ ir2asm ir
