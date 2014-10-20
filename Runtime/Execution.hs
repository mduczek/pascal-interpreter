module Execution where

import qualified Data.Map as Map
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans (liftIO)
import Text.Show.Functions
import AbsVar
import Data.List (intercalate)

type Loc = Int
type Env = Map.Map String EnvValue
type MutuallyRecursiveWith = [String]
data EnvValue = 
    EnvLoc Loc |
    EnvFun Function Env MutuallyRecursiveWith |
    EnvBuiltIn NativeFun |
    EnvRec RRecord
    deriving Show
type NativeFun = [StoreVal] -> Execution StoreVal
type RRecord = Map.Map String StoreVal

type Store = Map.Map Loc StoreVal
-- Allocator (freed locations to be used first) (the next loc if there are no free loc left)
data Allocator = Allocator [Int] Int deriving Show
data PState = PState Allocator Store deriving Show
data StoreVal = 
    StoreInt Int |
    StoreBool Bool |
    StoreString String |
    StoreArray (Map.Map Int StoreVal) |
    StoreTuple [StoreVal] |
    StoreRec RRecord
    deriving (Show,Eq)
--add map
--Ord?

--type Execution = StateT PState (ErrorT ErrorMsg IO)
type Execution = ReaderT Env (ErrorT ErrorMsg (StateT PState IO))
type ErrorMsg = String

--runExecution :: Execution a -> Int -> IO (Either ErrorMsg (a, PState))
runExecution :: Execution a -> Int -> IO (Either ErrorMsg a, PState)
runExecution k param =
    let state = PState (Allocator [] 1) Map.empty
        env = Map.fromList [("a",EnvLoc 10)]
    in  runStateT (runErrorT (runReaderT k env)) state


alloc :: StoreVal -> Execution Loc
alloc val = do
    (PState (Allocator freed newLoc) store) <- get
    case freed of
        [] -> do
            put (PState (Allocator freed (newLoc+1)) (Map.insert newLoc val store))
            return newLoc
        (loc : locs) -> do
            put (PState (Allocator locs newLoc) (Map.insert loc val store))
            return loc

dealloc :: [Loc] -> Execution ()
dealloc locs = do
    (PState (Allocator freed newLoc) store) <- get
    let freed' = locs ++ freed
        store' = foldl (\ acc l -> Map.delete l acc ) store locs
    put (PState (Allocator freed' newLoc) store')

storeGet :: Loc -> Execution StoreVal
storeGet loc = do
    (PState _ store) <- get
    case Map.lookup loc store of
            (Just val) -> return val
            Nothing -> throwError "Invalid location"

storePut :: Loc -> StoreVal -> Execution ()
storePut loc val = do
    (PState al store) <- get
    put (PState al (Map.insert loc val store))

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
            envfun@(EnvFun _ _ _) -> g envfun varName
            envfun@(EnvBuiltIn _ ) -> g envfun varName
            envrec@(EnvRec _) -> h envrec varName
        Nothing -> throwError $ "The " ++ typStr ++ " "++ varName ++" is undefined"


envGetFun :: String -> Execution EnvValue
envGetFun = envGetXX (envGetErr "variable") envGetOk (envGetErr "record") "function"

envGetRec :: String -> Execution EnvValue
envGetRec = envGetXX (envGetErr "variable") (envGetErr "function") envGetOk "record"

envGetLoc :: String -> Execution EnvValue
envGetLoc = envGetXX envGetOk (envGetErr "function") (envGetErr "record") "variable"

str :: StoreVal -> String
str (StoreInt n) = show n
str (StoreBool b) = show b
str (StoreArray a) = "[" ++ (intercalate ", " (fmap str (tail (Map.elems a)) ) )++ "]"
str (StoreTuple vs) = "(" ++ (intercalate ", " (fmap str vs) )++ ")"
str (StoreRec m) = concat $ fmap (\ (k, v) -> k ++ " -> " ++ str v ++ "; " )(Map.assocs m)
str (StoreString s) = "\"" ++ s ++ "\""

