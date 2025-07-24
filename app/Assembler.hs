module Assembler where

import Misc (Operand (..))
import Compile (IR (..))

data Asm = Mov Operand Operand |
           Call String | Lea Operand Operand |
           Label String | Ret | Sub Operand Operand |
           Pop Operand | Push Operand | Inline String |
           Comment String

-- Super big order of complexity find a nicer solution
movFolding :: [Asm] -> [Asm] -> [Asm]

movFolding (c@(Comment _):xs) acc =
  movFolding xs $ acc ++ [c]
movFolding (x:c@(Comment _):xs) acc =
  movFolding (x:xs) $ acc ++ [c]
movFolding (m1@(Mov y1 x):m2@(Mov z y2):xs) acc =
  if y1 == y2 then
   movFolding (Mov z x:xs) acc
  else movFolding (m2:xs) $ acc ++ [m1]

movFolding (x:xs) acc = movFolding xs $ acc ++ [x]
movFolding [] acc = acc

instance Show Asm where
  show (Mov x y) = "\n\tmov " ++ show x ++ ", " ++ show y
  show (Call label) = "\n\tcall " ++ label
  show (Lea x y) = "\n\tlea " ++ show x ++ ", " ++ show y
  show (Label label) = "\n" ++ label ++ ":"
  show Ret = "\n\tret"
  show (Pop x) = "\n\tpop " ++ show x
  show (Push x) = "\n\tpush " ++ show x
  show (Sub x y) = "\n\tsub " ++ show x ++ ", " ++ show y
  show (Inline x) = "\n\t" ++ x
  show (Comment s) = "\n\t; " ++ s

generateAsm :: [Asm] -> String
generateAsm asm = "global main\nsection .note.GNU-stack\nsection .text" ++ concatMap show optimised
  where optimised = movFolding asm []

generateData :: [String] -> String
generateData stringBank =
  "section .data\n" ++ (concatMap (\(n, s) -> dataEntry ((length stringBank - n), s)) $ zip [1..] stringBank)  
  where
    dataEntry :: (Int, String) -> String
    dataEntry (n, s) = "LC" ++ show n ++ ": db \"" ++ s ++ "\", 0\n" 

irComment :: IR -> Asm
irComment = Comment . show 

ir2asm :: IR -> [Asm]
ir2asm v@(Var _ size offset) = [irComment v, Mov StackPointer{offset=offset, size=size} Reg{suffix="ax", size=size}]
ir2asm lm@(LoadMemory size offset) = [irComment lm, Mov Reg{suffix="ax", size=size}  StackPointer{offset=offset, size=size}]
ir2asm e@(Enter label reserved) = [irComment e, Label label, Push $ Specific "rbp", Mov (Specific "rbp") (Specific "rsp"), Sub (Specific "rsp") . Specific $ show reserved]
ir2asm Leave = [irComment Leave, Mov (Specific "rsp") (Specific "rbp"), Pop (Specific "rbp"), Ret]
ir2asm m@(MovReg to from) = [irComment m, Mov to from]
ir2asm lr@(LoadVarRef offset size) = [irComment lr, Lea Reg{suffix="ax", size=8} StackPointer{offset=offset, size=size}]
ir2asm c@(AsmCall ident) = [irComment c, Call ident]
ir2asm (AsmInline asm) = [Comment "Inlined Assembly", Inline asm]
ir2asm (LoadRef x) = [Mov Reg{suffix="ax", size=8} $ Specific x]
