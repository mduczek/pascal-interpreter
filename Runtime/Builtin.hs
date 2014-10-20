module Builtin (builtinFuns) where

import Execution
import Control.Monad.Trans (liftIO)
import Control.Monad.Error
import Declarations
import qualified Data.Map as Map

--arrayTypeCheck = SExp (EAss (AVar (VarIndexed (VarSimple (Ident "array")) (EInt 0))) (VarSimple (Ident "a")))
fprint (a:[]) = do
    liftIO $ putStrLn (str a)
    return undefined

len ((StoreArray m):[]) = return $ StoreInt ((Map.size m)-1)
len _ = throwError "len only takes arrays"

fstr (a:[]) = return $ StoreString (str a)
fstr _ = throwError "??"

str_to_num ((StoreString a):[]) = return $ StoreInt (read a :: Int)
str_to_num _ = throwError "str_to_num only takes strings"

builtinFuns = [("?print",EnvBuiltIn fprint), ("?len", EnvBuiltIn len), ("?str", EnvBuiltIn fstr), ("?str_to_num", EnvBuiltIn str_to_num)]
