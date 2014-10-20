module Expressions (eval, evalBool, evalInt) where

import Execution
import AbsVar

eval :: Exp -> Execution StoreVal
evalBool :: Exp -> ErrorMsg -> Execution Bool
evalInt :: Exp -> ErrorMsg -> Execution Int