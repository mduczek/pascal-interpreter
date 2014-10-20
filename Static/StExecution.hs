module StExecution where

import qualified Data.Map as Map
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans (liftIO)
import AbsVar

type Env = Map.Map String EnvValue
data EnvValue = 
    EnvLoc Type |
    EnvFun Function Env |
    EnvRec RRecord
    deriving Show

type RRecord = Map.Map String Type

type Execution = ReaderT Env (ErrorT ErrorMsg IO)
type ErrorMsg = String

--runExecution :: Execution a -> Int -> IO (Either ErrorMsg (a, PState))
runExecution :: Execution a -> Int -> IO (Either ErrorMsg a)
runExecution k param =
    let env = Map.fromList [("a",EnvLoc TInt)]
    in  runErrorT (runReaderT k env)

getVarRoot :: Var -> String
getVarRoot (VarSimple (Ident name)) = name
getVarRoot (VarIndexed var _) = getVarRoot var
getVarRoot (VarFieldAccess var _) = getVarRoot var

envGetErr :: String -> EnvValue -> String -> Execution EnvValue
envGetErr typeName _ name = throwError $ "The " ++ name ++ "is a " ++ typeName ++ " name!"

envGetOk :: EnvValue -> String -> Execution EnvValue
envGetOk val _ = return val

envGetXX f g h typStr varName = do
    env <- ask
    case Map.lookup varName env of
        (Just x) -> case x of
            envLoc@(EnvLoc _) -> f envLoc varName
            envfun@(EnvFun _ _) -> g envfun varName
            envrec@(EnvRec _) -> h envrec varName
        Nothing -> throwError $ "The " ++ typStr ++ " "++ varName ++" is undefined"


envGetFun :: String -> Execution EnvValue
envGetFun = envGetXX (envGetErr "variable") envGetOk (envGetErr "record") "function"

envGetRec :: String -> Execution EnvValue
envGetRec = envGetXX (envGetErr "variable") (envGetErr "function") envGetOk "record"

envGetLoc :: String -> Execution EnvValue
envGetLoc = envGetXX envGetOk (envGetErr "function") (envGetErr "record") "variable"
