module StDeclarations (defineFuns, defineRecBlock, defineDecBlock, flattenDecls) where

import StExecution
import AbsVar
import Control.Monad.Error
import Control.Monad.Reader
import StStatements
import qualified Data.Map as Map

defineFuns :: [Function] -> Execution () -> Execution ()
defineFuns funs exec = do
    env <- ask
    nameNfuns <- sequence (fmap checkFun funs)
    let funDefs envi = fmap (\(n,f) -> (n, EnvFun f envi)) nameNfuns
        env1 = insertMulti (funDefs env) env
        -- a function within a block sees all functions from this block and higher 
        -- and also itself (recursion)
        env2 = insertMulti (funDefs env1) env1
    sequence_ $ fmap (\f -> local (const env2) (checkFunBody f) ) funs
    local (const env2) exec

checkFun ::  Function -> Execution (String, Function)
checkFun fun@(Fun (Ident name) args returnType decBlock innerFuns stms) = do
    case returnType of
        RetType typ -> checkTypePresent typ
        Void -> return ()
    checkDecls args
    return ("?" ++ name, fun)

checkFunBody :: Function -> Execution ()
checkFunBody fun@(Fun ident args returnType decBlock innerFuns stms) = do
    let args' = case returnType of
                    (RetType typ) -> (Dec [ident] typ) : args
                    Void -> args
        defineVars = defineDecBlock (DecBlockJust args') 
    defineVars $ defineDecBlock decBlock $ defineFuns innerFuns $ run (SComp stms)

checkDecls :: [Decl] -> Execution ()
checkDecls decls =
    sequence_ (fmap checkTypePresent types)
    where types = fmap (\ (Dec _ typ) -> typ ) decls

checkTypePresent :: Type -> Execution()
checkTypePresent (TArray t) = checkTypePresent t
checkTypePresent (TMap t1 t2) = do {checkTypePresent t1; checkTypePresent t2}
checkTypePresent (TRecord (Ident name)) = do {envGetRec name; return ()}
checkTypePresent TInt = return ()
checkTypePresent TBool = return ()
checkTypePresent TWildcard = return ()
checkTypePresent (TTuple t1 ts) = sequence_ (fmap checkTypePresent (t1:ts))
checkTypePresent TString = return ()

flattenDecls :: [Decl] -> [(String, Type)]
flattenDecls decls = concat $ fmap (\ (Dec idents typ) -> nameType typ idents ) decls
    where nameType typ = fmap (\ (Ident name) -> (name, typ) )

defineRec :: Record -> Execution () -> Execution ()
defineRec (Rec (Ident name) decls) exec = do
    --check if all the records it references are already loaded!
    checkDecls decls
    let rec = insertMulti (flattenDecls decls) Map.empty
    local (Map.insert name (EnvRec rec)) exec

defineRecBlock :: RecBlock -> Execution () -> Execution ()
defineRecBlock (RecBlockJust (rec:tl)) exec = 
    defineRec rec (defineRecBlock (RecBlockJust tl) exec)
defineRecBlock (RecBlockJust []) exec = exec
defineRecBlock RecBlockEmpty exec = exec

defineDecBlock :: DecBlock -> Execution () -> Execution ()
defineDecBlock (DecBlockJust (dec:tl)) exec = 
    defineDecl dec (defineDecBlock (DecBlockJust tl) exec)
defineDecBlock (DecBlockJust []) exec = exec
defineDecBlock DecBlockEmpty exec = exec

defineDecl :: Decl -> Execution () -> Execution ()
defineDecl (Dec idents typ) exec = do
    let nameLocs = fmap (\ (Ident name) -> (name, EnvLoc typ) ) idents
    local (insertMulti nameLocs) exec
    --dealloc locs

insertMulti :: Ord k => [(k,v)] -> Map.Map k v -> Map.Map k v
insertMulti l m = foldr (\(k,v) acc -> Map.insert k v acc) m l
