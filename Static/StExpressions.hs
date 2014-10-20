module StExpressions (eval, evalBool, evalInt) where

import StExecution
import AbsVar
import StStatements
import StDeclarations
import Control.Monad.Error
import qualified Data.Map as Map

cast :: Type -> Type -> Bool
cast TWildcard actual = True
cast (TArray expected) (TArray actual) = cast expected actual
cast (TTuple e1 es) (TTuple a1 as) = 
    foldr (\ (e,a) acc -> acc && (cast e a) ) True (zip (e1:es) (a1:as))
cast expected actual = expected == actual

eval :: Exp -> Execution Type

eval (EVar var) = update return var

eval (EAss (AVar var) e) = do
    typ <- eval e
    update (assign typ) var

eval (EAss (ATuple var varR) e) = do
    typ <- eval e
    let vars = var:varR
        typeMismatchMsg = "can't assign to tuple (" ++ (show vars) ++ ") type " ++ (show typ)
    case typ of
        (TTuple t1 ts) -> evalAssTuple vars (t1:ts)
        otherwise -> throwError typeMismatchMsg

eval (ETuple e1 es) = do
    (t1:ts) <- sequence $ fmap eval (e1:es)
    return $ TTuple t1 ts

eval (EConcat e1 e2) = evalBinary TString TString "++" e1 e2

eval (Eor e1 e2)  = evalBinary TBool TBool "||" e1 e2
eval (Eand e1 e2) = evalBinary TBool TBool "&&" e1 e2

eval (Eeq e1 e2)  = evalCmp e1 e2    
eval (Eneq e1 e2) = evalCmp e1 e2

eval (Elt e1 e2)   = evalBinary TInt TBool "<"  e1 e2
eval (Elte e1 e2)  = evalBinary TInt TBool "<=" e1 e2
eval (Egt e1 e2)   = evalBinary TInt TBool ">"  e1 e2
eval (Egte e1 e2)  = evalBinary TInt TBool ">=" e1 e2
eval (EAdd e1 e2)  = evalBinary TInt TInt "+" e1 e2
eval (ESub e1 e2)  = evalBinary TInt TInt "-" e1 e2
eval (EMul e1 e2)  = evalBinary TInt TInt "*" e1 e2
eval (EDiv e1 e2)  = evalBinary TInt TInt "/" e1 e2

eval (EBNeg e) = do
    t <- eval e
    if t == TBool then return TBool else throwError $ "can't negate " ++ (show t)

eval (EMinus e) = do
    t <- eval e
    if t == TInt then return TInt else throwError $ "can't apply minus to " ++ (show t)

eval (EInt n) = return TInt
eval (EStr s) = return TString
eval Etrue = return TBool
eval Efalse = return TBool

eval (Call (Ident name) es) = do
    (EnvFun (Fun _ decls retType decB funs stms) env) <- envGetFun ("?" ++ name)
    let expectedTypes = fmap snd (flattenDecls decls)
        check (t1, t2, e2) = 
            if cast t1 t2 then return () 
                else throwError $ (show e2)++" should be of type "++(show t1)++" but is "++(show t2)
    actualTypes <- sequence (fmap eval es)
    if length expectedTypes /= length actualTypes then 
        throwError $ name++" called with invalid number of args, should be "++(show expectedTypes)
        else return ()
    sequence_ $ fmap check (zip3 expectedTypes actualTypes es)
    case retType of
        (RetType rTyp) -> return rTyp
        Void -> return $ TRecord (Ident "void")

eval (ECast e t) = return t

eval e = throwError $ "can't eval " ++ (show e)

evalCmp :: Exp -> Exp -> Execution Type
evalCmp e1 e2  = do
    t1 <- eval e1
    t2 <- eval e2
    if (cast t1 t2) || (cast t2 t1)
        then return TBool
        else throwError $ "can't compare " ++ (show t1) ++ " with " ++ (show t2) 

type OpStr = String

evalBinary :: Type -> Type -> OpStr -> Exp -> Exp -> Execution Type
evalBinary typIn typOut opStr e1 e2 = do
    t1 <- eval e1
    t2 <- eval e2
    if t1 == t2 && t1 == typIn
        then return typOut
        else
            let str e t = " "++(show e) ++ ":" ++ (show t)++" "
            in throwError $ "invalid types in expr" ++ (str e1 t1) ++ opStr ++ (str e2 t2)

type Updater =  Type -> Execution Type

assign :: Type -> Updater
assign actual expected = do
    if cast expected actual then
        return expected
    else throwError $ "can't assign " ++ (show actual) ++ " to " ++ (show expected)

update :: Updater -> Var -> Execution Type
update f var = do
    let revrse vs@(VarSimple ident) path = vs:path
        revrse vi@(VarIndexed var e) path  = revrse var (vi:path)
        revrse vf@(VarFieldAccess var ident) path = revrse var (vf:path)
        full@((VarSimple (Ident name)) : path) = revrse var []
    (EnvLoc rootType) <- envGetLoc name
    up f rootType full

type VarAccess = [Var] -- eg a[1].b.d[2]
up :: Updater -> Type -> VarAccess -> Execution Type
up f t ((VarSimple _) : []) = f t

up f t ((VarSimple _) : tl) = up f t tl

up f (TArray typ) ((VarIndexed _ e) : tl) = do
    case tl of
        [] -> f typ
        otherwise -> up f typ tl

up f (TRecord (Ident name)) ((VarFieldAccess _ (Ident field)) : tl) = do
    (EnvRec rec) <- envGetRec name
    case Map.lookup field rec of
        (Just typ) -> do
            case tl of
                [] -> f typ
                otherwise -> up f typ tl
        otherwise -> throwError $ "no field " ++ field ++ " in record " ++ name

up _ b c = throwError $  "up " ++ (show b) ++ " " ++ (show c)

evalAssTuple :: [Var] -> [Type] -> Execution Type
evalAssTuple vars types = do
    if (length vars == length types) then do 
        let typeVars = zip types vars
        (t1:ts) <- sequence $ fmap (\ (typ',var') -> update (assign typ') var' ) typeVars
        return $ TTuple t1 ts
    else throwError $ "can't assign to tuple (" ++ (show vars) ++ ") type " ++ (show types)

--helper functions

evalInt :: Exp -> ErrorMsg -> Execution ()
evalInt e errorMsg = do
    typ <- eval e
    if typ == TInt then return () else throwError errorMsg

evalBool :: Exp -> ErrorMsg -> Execution ()
evalBool e errorMsg = do
    typ <- eval e
    if typ == TBool then return () else throwError errorMsg