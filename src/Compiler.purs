module Compiler where

import Asm (Arg(..), Inst(..), Reg(..))
import Data.Either (Either(..))
import Data.Map (Map, empty, insert, lookup, size)
import Data.Maybe (maybe)
import Data.String.Yarn (unlines)
import Data.Tuple (Tuple(..))
import Expr (Expr(..), Identifier)
import Prelude (class Applicative, class Semigroup, bind, map, pure, show, ($), (+), (<$>), (<*>), (<<<), (<>))

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

compileExpr :: Expr -> Env -> Compilation
compileExpr (Number n)      _   = pure [ IMov (Reg EAX) (Const n) ]
compileExpr (Add1 exp)      env = (compileExpr exp env) <||> (pure [ Add (Reg EAX) (Const 1) ])
compileExpr (Sub1 exp)      env = (compileExpr exp env) <||> (pure [ Sub (Reg EAX) (Const 1) ])
compileExpr (Identifier id) env = 
    do stackIndex <- lookupIdentifier id env
       pure [ IMov (Reg EAX)  (RegOffset ESP stackIndex) ]
compileExpr (Let id varExpr bodyExpr) env =
    let (Tuple env' stackIndex) = addIdentifier id env
    in (compileExpr varExpr env) <||> 
        (pure [ IMov (RegOffset ESP stackIndex) (Reg EAX) ]) <||>
        (compileExpr bodyExpr env')

addTab :: String -> String
addTab s = "  " <> s

initialEnv :: Env
initialEnv = empty

compileProg :: Expr -> Either String String
compileProg expr = 
    do instrs  <- compileExpr expr initialEnv 
       let prelude = unlines [ "section .text"
                          , "global our_code_starts_here"
                          , "our_code_starts_here:"
                          ]
       let asm     = unlines $ map (addTab <<< show) instrs
       let end     = addTab "ret"
       pure $ prelude <> "\n" <> asm <> "\n" <> end