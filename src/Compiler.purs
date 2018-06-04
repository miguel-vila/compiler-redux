module Compiler where

import Asm (Arg(..), Inst(..), Reg(..), Label, instToString)
import Data.Either (Either(..))
import Data.Map (Map, empty, insert, lookup, size)
import Data.Maybe (maybe)
import Data.String.Yarn (unlines)
import Data.Tuple (Tuple(..))
import Expr (Binding(..), Expr(..), Identifier, Prim1(..), Tag, tag)
import Prelude (class Applicative, class Semigroup, bind, map, pure, show, ($), (+), (<$>), (<*>), (<<<), (<>), (=<<))

type StackIndex = Int

type Env = Map Identifier StackIndex

type Compilation = Either String (Array Inst)

lookupIdentifier :: Identifier -> Env -> Either String StackIndex
lookupIdentifier id env =
    maybe (Left $ "Didn't find a var with name " <> id) Right (lookup id env) 

addIdentifier :: Identifier -> Env -> (Tuple Env StackIndex)
addIdentifier id env =
    let slot = size env + 1
    in Tuple (insert id slot env) slot

infixl 4 appAppend as <||>

appAppend :: forall f a. Applicative f => Semigroup a => f a -> f a -> f a
appAppend x y = (<>) <$> x <*> y

compilePrim1 :: Prim1 -> Expr Tag -> Env -> Compilation
compilePrim1 prim1 arg env =(compileExpr arg env) <||> (pure operation)
    where operation = case prim1 of
            Add1 -> [ Add (Reg EAX) (Const 1) ]
            Sub1 -> [ Sub (Reg EAX) (Const 1) ]

compileExpr :: Expr Tag -> Env -> Compilation
compileExpr (Number n _)      _   = pure [ IMov (Reg EAX) (Const n) ]
compileExpr (EPrim1 prim1 arg _) env = compilePrim1 prim1 arg env
compileExpr (Identifier id _) env = 
    do stackIndex <- lookupIdentifier id env
       pure [ IMov (Reg EAX)  (RegOffset ESP stackIndex) ]
compileExpr (Let (Binding id varExpr _) bodyExpr _) env =
    let (Tuple env' stackIndex) = addIdentifier id env
    in (compileExpr varExpr env) <||> 
        (pure [ IMov (RegOffset ESP stackIndex) (Reg EAX) ]) <||>
        (compileExpr bodyExpr env')
compileExpr (If cond ifT ifF tag) env = 
    (compileExpr cond env) <||>
    (pure [ Cmp (Reg EAX) (Const 0)
          , JE ifFLabel
          ]) <||>
    (compileExpr ifT env) <||>
    (pure [ Jmp doneLabel 
          , Label ifFLabel ]) <||>
    (compileExpr ifF env) <||>
    (pure [ Label doneLabel ])
    where ifFLabel  = "if_false_" <> show tag
          doneLabel = "done_" <> show tag

-- isImm :: forall a. Expr a -> Boolean
-- isImm (Number _ _)     = true
-- isImm (Identifier _ _) = true
-- isImm _                = false

-- anf :: Expr Label -> Expr Label
-- anf eprim1 @ (EPrim1 prim1 arg tag) = 
--     if isImm arg
--     then eprim1 
--     else let id = "tmp_" <> show tag
--          in Let (Binding id (anf arg) tag)
--                 (EPrim1 prim1 (Identifier id tag) tag)
--                 tag
-- anf eprim2 @ (EPrim2 prim2 arg1 arg2 tag) =
-- -- @TODO refactor this branch
--     if isImm arg1 then
--         if isImm arg2 then
--             eprim2
--         else
--             let id2 = "tmp_" <> show tag <> "_2"
--             in Let (Binding id2 (anf arg2) tag) 
--                    (EPrim2 prim2 arg1 (Identifier id2 tag) tag)
--                    tag
--     else
--         if isImm arg2 then
--             let id1 = "tmp_" <> show tag <> "_1"
--             in Let (Binding id1 (anf arg1) tag) 
--                    (EPrim2 prim2 (Identifier id1 tag) arg2 tag)
--                    tag
--         else
--             let id1 = "tmp_" <> show tag <> "_1"
--                 id2 = "tmp_" <> show tag <> "_2"
--             in Let (Binding id1 (anf arg1) tag)
--                    (Let (Binding id2 (anf arg2) tag)
--                         (EPrim2 prim2 (Identifier id1 tag) (Identifier id2 tag) tag)
--                         tag)
--                    tag
-- anf (Let binding body tag) = Let (anfBind binding) (anf body) tag
--     where anfBind (Binding identifier expr tag) = Binding identifier (anf expr) tag
-- anf ife@(If cond ifTrue ifFalse tag) =
--     if isImm cond then If cond (anf ifTrue) (anf ifFalse) tag
--     else Let (Binding "cond" (anf cond) tag) (If (Identifier "cond" tag) (anf ifTrue) (anf ifFalse) tag) tag
-- anf expr = expr

addTab :: String -> String
addTab s = "  " <> s

initialEnv :: Env
initialEnv = empty

compileProg :: forall a. Expr a -> Either String String
compileProg expr = 
    do let tagged = tag expr
       instrs  <- compileExpr tagged initialEnv 
       let prelude = unlines [ "section .text"
                             , "global our_code_starts_here"
                             , "our_code_starts_here:"
                             ]
       let asm     = unlines $ map (addTab <<< instToString) instrs
       let end     = addTab "ret"
       pure $ prelude <> "\n" <> asm <> "\n" <> end