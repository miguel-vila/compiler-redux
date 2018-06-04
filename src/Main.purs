module Main where

import Compiler (compileProg)
import Control.Monad (class Functor, pure, void, (*>), (=<<))
import Control.Monad.Aff (Aff, Canceler, Error, catchError, makeAff, nonCanceler, runAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (message)
import Data.Either (Either(..), either)
import Data.Functor (voidRight)
import Data.Maybe (maybe)
import Expr (Binding(..), Expr(..), Prim1(..))
import Node.Buffer (BUFFER, Buffer, toString)
import Node.ChildProcess (CHILD_PROCESS, defaultExecOptions, exec)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (writeTextFile)
import Node.Process (PROCESS, exit)
import Prelude (Unit, bind, unit, ($), (<<<), (>>>))

addNonCanceler :: forall a f eff. Functor f => f a -> f (Canceler eff)
addNonCanceler = voidRight nonCanceler

execA :: forall eff. String -> Aff (cp :: CHILD_PROCESS | eff) Buffer
execA = makeAff <<< execEitherCb 
  where execEitherCb c cb = execWithCanceller c (\execResult -> cb (maybe (Right execResult.stdout) Left (execResult.error))) 
        execWithCanceller c cb = addNonCanceler (exec c defaultExecOptions cb)

execA_ :: forall eff. String -> Aff (cp :: CHILD_PROCESS | eff) Unit
execA_ = void <<< execA

exitA ::forall eff a. Int -> Aff ( process :: PROCESS | eff ) a
exitA = liftEff <<< exit

logA :: forall eff. String -> Aff ( console :: CONSOLE | eff ) Unit
logA = liftEff <<< log

toStringA :: forall eff. Buffer -> Aff ( buffer :: BUFFER | eff) String
toStringA = liftEff <<< toString UTF8

compileA :: forall eff a. Expr a -> Aff ( process :: PROCESS, console :: CONSOLE | eff ) String
compileA expr = either (\err -> (logA err) *> (exitA 1)) pure (compileProg expr)

main :: forall e. Eff ( buffer :: BUFFER, process :: PROCESS, cp :: CHILD_PROCESS, console :: CONSOLE, fs :: FS | e) Unit
main = runAff_ (either printError printSuccess) $ do
  -- let expr = Let (Binding "x" (Sub1 (Number 5 unit) unit) unit) (Add1 (Add1 (Identifier "x" unit) unit) unit) unit
  let expr = If (EPrim1 Sub1 (Number 2 unit) unit) (Number 10 unit) (Number 20 unit) unit
  -- let expr = Crash 5 unit
  compiled <- compileA expr
  _ <- writeTextFile UTF8 "./compiled.s" compiled `catchError` logAndExit
  _ <- logA "Compiled!"
  _ <- (execA_ "nasm -f elf32 -o compiled.o compiled.s") `catchError` logAndExit
  _ <- logA "Assembled!"
  _ <- (execA_ "clang -g -m32 -o code runtime/runtime.c compiled.o") `catchError` logAndExit
  _ <- logA "Linked!"
  _ <- logA "Running..."
  _ <- (logA =<< toStringA =<< execA "./code") `catchError` logAndExit
  pure unit
  where onExec successMsg execResult = maybe (log successMsg) printError execResult.error
        printError :: forall eff. Error -> Eff ( console :: CONSOLE | eff ) Unit
        printError = message >>> log
        printError' :: forall eff. Error -> Aff ( console :: CONSOLE | eff ) Unit
        printError' = message >>> logA
        printSuccess _ = log "Successful run!"
        logAndExit err = (printError' err) *> (exitA 1)
