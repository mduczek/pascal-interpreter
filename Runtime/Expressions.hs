module Expressions (eval, evalBool, evalInt) where

import Execution
import AbsVar
import Statements
import Declarations
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as Map

eval :: Exp -> Execution StoreVal

eval (EVar var) = update return var

eval (EAss (AVar var) e) = do
    val <- eval e
    update (assign val) var

eval (EAss (ATuple var varR) e) = do
    val <- eval e
    let vars = var:varR
        typeMismatchMsg = "can't assign to tuple " ++ (show vars)
    vals <- case val of
                (StoreTuple vals') -> return vals'
                otherwise -> throwError typeMismatchMsg
    evalAssTuple vars vals

eval (ETuple e1 es) = do
    vs <- sequence $ fmap eval (e1:es)
    return $ StoreTuple vs

eval (EConcat e1 e2) = evalBinary unpackStr StoreString TString (++) "++" e1 e2

eval (Eor e1 e2)  = evalBinary unpackBool StoreBool TBool (||) "||" e1 e2
eval (Eand e1 e2) = evalBinary unpackBool StoreBool TBool (&&) "&&" e1 e2

eval (Eeq e1 e2)  = evalCmp (==) e1 e2    
eval (Eneq e1 e2) = evalCmp (/=) e1 e2

eval (Elt e1 e2)   = evalBinary unpackInt StoreBool TBool (<) "<" e1 e2
eval (Elte e1 e2)  = evalBinary unpackInt StoreBool TBool (<=) "<=" e1 e2
eval (Egt e1 e2)   = evalBinary unpackInt StoreBool TBool (>) ">" e1 e2
eval (Egte e1 e2)  = evalBinary unpackInt StoreBool TBool (>=) ">=" e1 e2
eval (EAdd e1 e2)  = evalBinary unpackInt StoreInt TInt (+) "+" e1 e2
eval (ESub e1 e2)  = evalBinary unpackInt StoreInt TInt (-) "-" e1 e2
eval (EMul e1 e2)  = evalBinary unpackInt StoreInt TInt (*) "*" e1 e2
eval (EDiv e1 e2)  = evalBinary unpackInt StoreInt TInt div "/" e1 e2

eval (EInt n) = return (StoreInt (fromInteger n))
eval Etrue = return (StoreBool True)
eval Efalse = return (StoreBool False)
eval (EStr s) = return (StoreString s)

eval (Call (Ident fname) es) = do
    f <- envGetFun ("?"++fname)
    vals <- sequence $ fmap eval es
    evalFun fname f vals

eval (ECast e _) = eval e

eval (EMinus e) = do
    sval <- eval e
    val <- unpackInt sval "unary -"
    return (StoreInt (-val))

eval (EBNeg e) = do
    sval <- eval e
    val <- unpackBool sval "!"
    return (StoreBool (not val))

eval e = throwError $ "can't eval" ++ (show e)

evalFun :: String -> EnvValue -> [StoreVal] -> Execution StoreVal

evalFun fname (EnvFun (Fun _ decls retType decB funs stms) env recursives) vals = do
    envFuns <- sequence $ fmap envGetFun recursives
    let exec = defineDecBlock decB $ defineFuns funs $ run (SComp stms)
        envRec = insertMulti (zip recursives envFuns) env
        env0 nms lcs = insertMulti (zip nms (fmap EnvLoc lcs)) envRec
        enriched nms lcs = local (const (env0 nms lcs)) exec
        names = fmap fst (flattenDecls decls) 
    case retType of
        Void -> do
            locs <- sequence $ fmap alloc vals
            enriched names locs
            dealloc locs
            return undefined
        (RetType typ) -> do
            v <- zero typ
            locs <- sequence $ fmap alloc (v:vals)
            enriched (fname:names) locs
            returnVal <- storeGet (head locs)
            dealloc locs
            return returnVal

evalFun _ (EnvBuiltIn exec) vals = exec vals

evalCmp :: (StoreVal->StoreVal->Bool) -> Exp -> Exp -> Execution StoreVal
evalCmp op e1 e2  = do
    v1 <- eval e1
    v2 <- eval e2
    return (StoreBool (op v1 v2))

unpackInt :: StoreVal -> OpStr -> Execution Int
unpackInt (StoreInt n) _ = return n
unpackInt sval opStr = 
    throwError ("In the operand to " ++ opStr ++ " expected Int but was " ++ (str sval) )

unpackBool :: StoreVal -> OpStr -> Execution Bool
unpackBool (StoreBool b) _ = return b
unpackBool sval opStr = 
    throwError ("In the operand to " ++ opStr ++ " expected Bool but was " ++ (str sval) )

unpackStr :: StoreVal -> OpStr -> Execution String
unpackStr (StoreString s) _ = return s
unpackStr sval opStr = 
    throwError ("In the operand to " ++ opStr ++ " expected String but was " ++ (str sval) )


type OpStr = String

evalBinary :: (StoreVal -> OpStr -> Execution a) -> (b -> StoreVal) -> Type -> (a -> a -> b) -> OpStr -> Exp -> Exp -> Execution StoreVal
evalBinary unpack pack typ op opStr e1 e2 = do
    sval1 <- eval e1
    sval2 <- eval e2
    val1 <- unpack sval1 opStr
    val2 <- unpack sval2 opStr
    return $ pack (op val1 val2)

type Updater =  StoreVal -> Execution StoreVal

assign :: StoreVal -> Updater
assign vNew vOld = return vNew

update :: Updater -> Var -> Execution StoreVal
update f var = do
    let revrse vs@(VarSimple ident) path = vs:path
        revrse vi@(VarIndexed var e) path  = revrse var (vi:path)
        revrse vf@(VarFieldAccess var ident) path = revrse var (vf:path)
        full@((VarSimple (Ident name)) : path) = revrse var []
    (EnvLoc loc) <- envGetLoc name
    val <- storeGet loc
    (val',ret) <- up f val full
    storePut loc val'
    --debug
    return ret

type VarAccess = [Var] -- eg a[1].b.d[2]
type ReturnVal = StoreVal
up :: Updater -> StoreVal -> VarAccess -> Execution (StoreVal, ReturnVal)

up f v ((VarSimple _) : []) = twins (f v)

up f v ((VarSimple _) : tl) = up f v tl

up f (StoreArray vs) ((VarIndexed _ e) : tl) = do
    index <- evalInt e "the index is not a number"
    if index < 1 then throwError "index must be >= 1" else return ()
    case Map.lookup index vs of
        (Just val) -> do
            (val', ret) <- case tl of
                            [] -> twins (f val)
                            otherwise -> up f val tl
            return (StoreArray (Map.insert index val' vs), ret)
        Nothing -> throwError $ "Index out of bounds: " ++ (show index) ++ " where length is " ++ (show ((Map.size vs)-1))

up f (StoreRec rec) ((VarFieldAccess _ (Ident field)) : tl) = do
    case Map.lookup field rec of
        (Just val) -> do
            (val', ret) <- case tl of --the return type must be the same as 'typ'!
                            [] -> twins (f val)
                            otherwise -> up f val tl
            return (StoreRec (Map.insert field val' rec), ret)
        otherwise -> throwError $ "Whoa! no field " ++ field ++ " in record " ++ (show rec)

up _ val var = throwError $  "Trying to get "++(show var)++" but value is "++(str val) 

twins :: Execution a -> Execution (a,a)
twins exec = exec >>= (\v -> return (v,v))

evalAssTuple :: [Var] -> [StoreVal] -> Execution StoreVal
evalAssTuple vars vals = do
    let typeMismatchMsg = "can't assign to tuple (" ++ (show vars) ++ ") type " ++ (show vals)
    if (length vals == length vars) then do 
        let valVars = zip vals vars
        res <- sequence $ fmap (\ (val',var') -> update (assign val') var' ) valVars
        return $ StoreTuple res
    else throwError typeMismatchMsg

--helper functions

evalInt :: Exp -> ErrorMsg -> Execution Int
evalInt e errorMsg = do
    res <- eval e
    case res of
        (StoreInt n) -> return n
        otherwise -> throwError errorMsg

evalBool :: Exp -> ErrorMsg -> Execution Bool
evalBool e errorMsg = do
    res <- eval e
    case res of
        (StoreBool b) -> return b
        otherwise -> throwError errorMsg

trace exec = do
    res <- exec
    liftIO $ putStrLn (show res)
    return res

_debug = True

debug :: Execution()
debug = if _debug 
    then do
        env <- ask
        state <- get
        liftIO $ putStrLn (show env)
        liftIO $ putStrLn (show state)
    else return ()

