module StExpressions (eval, evalBool, evalInt) where

import StExecution
import AbsVar

eval :: Exp -> Execution Type
evalBool :: Exp -> ErrorMsg -> Execution ()
evalInt :: Exp -> ErrorMsg -> Execution ()