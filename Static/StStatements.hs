module StStatements (run) where

import {-# SOURCE #-} StExpressions
import StExecution
import AbsVar
import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as Map

run :: Stm -> Execution ()
run (SComp stms) = sequence_ (fmap run stms)

run (SExp e) = do
    eval e
    return ()

run (SIte e stm elifs elseStm) = do
    evalBool e "the value in if must be bool"
    run stm
    run (SComp (fmap (\ (Eliff e stm) -> stm ) elifs))
    sequence_ $ fmap (\ (Eliff e stm) -> evalBool e "the value in elif must be bool" ) elifs
    run elseStm

run (SIt e stm elifs) = run (SIte e stm elifs (SExp Efalse))

run (SWhile e stm) = do
    evalBool e "the value in if must be bool"
    run stm

run (SFor e1 e2 e3 stm) = do
    eval e1
    eval e2
    eval e3
    run stm

run (SLength (Ident name) e) = do
    (EnvLoc typ) <- envGetLoc name
    case typ of
        (TArray _) -> return ()
        otherwise -> throwError $ name ++ " is not an array"
    evalInt e "the value in length statement must be int"