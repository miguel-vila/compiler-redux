module Asm where

import Prelude (class Show, show, (<>))

data Reg = EAX
         | ESP

instance showReg :: Show Reg where
  show EAX = "eax"
  show ESP = "esp"

data Arg = Const Int
         | Reg Reg
         | RegOffset Reg Int

instance showArg :: Show Arg where
  show (Const n)         = show n
  show (Reg reg)         = show reg
  show (RegOffset reg n) = "[" <> show reg <> " - 4*" <> show n <> "]"

data Inst = IMov Arg Arg
          | Add Arg Arg
          | Sub Arg Arg

binOpStr :: String -> Arg -> Arg -> String
binOpStr op arg1 arg2 = op <> " " <> show arg1 <> ", " <> show arg2

instance showInst :: Show Inst where
  show (IMov arg1 arg2) = binOpStr "mov" arg1 arg2
  show (Add  arg1 arg2) = binOpStr "add" arg1 arg2
  show (Sub  arg1 arg2) = binOpStr "sub" arg1 arg2