module Asm where

import Prelude ((<>), show)

data Reg = EAX
         | ESP

regToString :: Reg -> String
regToString EAX = "eax"
regToString ESP = "esp"

data Arg = Const Int
         | Reg Reg
         | RegOffset Reg Int

argToString :: Arg -> String
argToString (Const n)         = show n
argToString (Reg reg)         = regToString reg
argToString (RegOffset reg n) = "[" <> regToString reg <> " - 4*" <> show n <> "]"

type Label = String

data Inst = IMov Arg Arg
          | Add Arg Arg
          | Sub Arg Arg
          | Cmp Arg Arg
          | Jmp Label
          | JE Label
          | Label Label

binOpStr :: String -> Arg -> Arg -> String
binOpStr op arg1 arg2 = op <> " " <> argToString arg1 <> ", " <> argToString arg2

instToString :: Inst -> String
instToString (IMov arg1 arg2) = binOpStr "mov" arg1 arg2
instToString (Add  arg1 arg2) = binOpStr "add" arg1 arg2
instToString (Sub  arg1 arg2) = binOpStr "sub" arg1 arg2
instToString (Cmp  arg1 arg2) = binOpStr "cmp" arg1 arg2
instToString (Jmp  label    ) = "jmp " <> label  
instToString (JE   label    ) = "je " <> label
instToString (Label label   ) = label <> ":"