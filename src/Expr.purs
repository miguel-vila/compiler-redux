module Expr where

data Expr = Number Int
          | Add1 Expr
          | Sub1 Expr
          | Identifier Identifier
          | Let Identifier Expr Expr
          | If Expr Expr Expr

type Identifier = String
