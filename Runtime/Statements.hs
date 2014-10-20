module Statements (run) where

import {-# SOURCE #-} Expressions
import Execution
import AbsVar
import Declarations (insertMulti)
import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as Map

run :: Stm -> Execution ()
run (SComp stms) = sequence_ (fmap run stms)

run (SExp e) = do
    eval e
    return ()

run (SIte e stm elifs elseStm) = do
    b <- evalBool e "the value in if must be bool"
    if b then run stm
    else do
        matched <- runElifs elifs
        if matched then return () else run elseStm

run (SIt e stm elifs) = run (SIte e stm elifs (SExp Efalse))

run while@(SWhile e stm) = do
    b <- evalBool e "the value in if must be bool"
    if b
        then do
            run stm
            run while
        else return ()

run (SFor e1 e2 e3 stm) = do
    eval e1
    run (SWhile e2 (SComp [stm, SExp e3]))

run (SLength (Ident name) e) = do
    (EnvLoc loc) <- envGetLoc name
    (StoreArray arr) <- storeGet loc
    len <- evalInt e "the value in length statement must be int"
    if len < 1 then throwError "length must be >= 1" else return ()
    case Map.lookup 0 arr of
        (Just vzero) -> 
            let len0 = Map.size arr
                arr' =  if len >= len0 then
                            let indices = [len0..len]
                                vzeros = fmap (const vzero) indices
                            in insertMulti (zip indices vzeros) arr
                        else
                            foldr Map.delete arr [(len+1)..len0]
            in storePut loc (StoreArray arr')
        Nothing -> throwError "?!!?"

runElifs :: [Elif] -> Execution Bool
runElifs [] = return False
runElifs (elif : tl) = do
    matched <- runElif elif
    if matched then return True else runElifs tl

runElif :: Elif -> Execution Bool
runElif (Eliff e stm) = do
    b <- evalBool e "the value in if must be bool"
    if b then do {run stm; return True} else return False

