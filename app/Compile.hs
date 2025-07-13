module Compile where

import Parse (Token (..))
import GHC.Float (float2Int)
import Data.Bits ((.&.))  

data IR = FrameFunc String Word [IR] |
          LeafFunc String [IR] |
          Variable String Word Word Int |
          Call String [IR] |
          Inline String | Immediate Int

token2ir :: Token -> Maybe IR
token2ir (Expr ((Ident "define"):xs)) =
  case xs of
    [Ident label, Num x] -> Just . Variable label 4 0 $ float2Int x 
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
token2ir (Expr ((Ident fname):xs)) = do
  ys <- mapM token2ir xs
  Just $ Call fname ys
token2ir _ = Nothing

instr2asm :: IR -> String
instr2asm (Immediate x) = "\nmov rax, " <> show x
instr2asm (Variable ident bytes rbp x)
  = "\nmov DWORD [rbp-" <> show rbp <> "], " <> show x
instr2asm (FrameFunc name space body) 
  = "\n" <> name <> ":\npush rbp\nmov rbp, rsp\nsub rsp, " <> show space <> ir2asm body <> "\nleave\nret"
instr2asm (Inline asm) = "\n" <> asm
instr2asm (Call fname args) = "\ncall " <> fname

-- TODO: Implement function calls 
-- instr2asm (Call fname args) _ = (buildParams args <> "call " <> fname, 0)
--   where
--     paramRegister = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
--     buildParams :: [IR] -> String
--     buildParams = setupParams . zip . paramRegister 

--     setupParams :: [(String, IR)] -> String
--     setupParams x = setupParams' x ""
    
--     setupParams' :: [(String, IR)] -> String -> String
--     setupParams' [] acc = acc
--     setupParams' ((reg, ir):xs) acc = setupParams xs
--      $ acc <> (fst $ instr2asm ir) <> "\nmov " <> reg <> " rax"

instr2asm _ = ""

ir2asm :: [IR] -> String
ir2asm = foldr (\x acc -> instr2asm x <> acc) ""

compile :: [Token] -> Maybe String
compile tokens = do
  ir <- mapM token2ir tokens
  Just $ (<>) "global _start\nsection .text" $ ir2asm ir
