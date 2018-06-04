module Expr where

import Prelude

import Data.Tuple (Tuple(..), fst)

data Binding a = Binding Identifier (Expr a) a

data Prim1 = Add1
           | Sub1

data Prim2 = Plus
           | Minus
           | Times

data Expr a = Number Int a
            | EPrim1 Prim1 (Expr a) a
            -- | EPrim2 Prim2 (Expr a) (Expr a) a
            | Identifier Identifier a
            | Let (Binding a) (Expr a) a
            | If (Expr a) (Expr a) (Expr a) a

getTag :: forall a. Expr a -> a
getTag (Number     _ t) = t
getTag (EPrim1   _ _ t) = t
-- getTag (EPrim2 _ _ _ t) = t
getTag (Identifier _ t) = t
getTag (Let      _ _ t) = t
getTag (If     _ _ _ t) = t

type Tag = Int

next :: Tag -> Tag
next x = x+1

tag :: forall a. Expr a -> Expr Tag
tag = fst <<< (go 0)
    where go tag (Number n _                    ) = 
            Tuple (Number n tag) (next tag) 
          go tag (EPrim1 prim1 expr _           ) = 
            let Tuple expr' nextTag = go (next tag) expr
            in Tuple (EPrim1 prim1 expr' tag) nextTag
          -- go tag (EPrim2 prim2 arg1 arg2 _      ) = 
          --   let Tuple arg1' tag'  = go (next tag ) arg1
          --       Tuple arg2' tag'' = go (next tag') arg2
          --   in Tuple (EPrim2 prim2 arg1' arg2' tag) tag''
          go tag (Identifier id _               ) = 
            Tuple (Identifier id tag) (next tag)
          go tag (Let (Binding id bind _) body _) = 
            let Tuple bind' tag'  = go (next tag ) bind
                Tuple body' tag'' = go (next tag') body
            in Tuple (Let (Binding id bind' tag) body' tag) tag''
          go tag (If cond ifT ifF _             ) = 
            let Tuple cond' tag'   = go (next tag  ) cond
                Tuple ifT'  tag''  = go (next tag' ) ifT
                Tuple ifF'  tag''' = go (next tag'') ifF
            in Tuple (If cond' ifT' ifF' tag) tag'''

type Identifier = String
