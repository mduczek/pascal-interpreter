module Declarations (defineFuns, defineRecBlock, defineDecBlock, defineBuiltin, 
    flattenDecls, zero, insertMulti) where

import Execution
import AbsVar
import Control.Monad.Error
import Control.Monad.Reader
import qualified Data.Map as Map

defineBuiltin :: [(String, EnvValue)] -> Execution () -> Execution ()
defineBuiltin envV exec = local (insertMulti envV) exec

defineFuns :: [Function] -> Execution () -> Execution ()
defineFuns funs exec = do
    env <- ask
    let names = fmap funName funs
        funDefs envi = fmap (\(n,f) -> (n, EnvFun f envi names)) (zip names funs)
        env1 = insertMulti (funDefs env) env
    local (const env1) exec

funName :: Function -> String
funName (Fun (Ident name) _ _ _ _ _) = "?" ++ name

defineRec :: Record -> Execution () -> Execution ()
defineRec rec@(Rec (Ident name) decls) exec = do
    idsVals <- zeros decls
    let fields = foldl (\ acc (ident, val) -> Map.insert ident val acc ) Map.empty idsVals
    local (Map.insert name (EnvRec fields)) exec

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

zero :: Type -> Execution StoreVal
zero TInt = return (StoreInt 0)
zero TBool = return (StoreBool False)
zero TString = return (StoreString "")
zero TWildcard = return (StoreInt 0)
zero (TArray t) = do
    v0 <- zero t
    return (StoreArray (Map.insert 0 v0 Map.empty))
    
zero (TRecord (Ident name)) = do
    (EnvRec emptyRec) <- envGetRec name
    return $ StoreRec emptyRec

zero (TTuple t ts) = do
    v0 <- sequence $ fmap zero (t:ts)
    return (StoreTuple v0)

flattenDecls :: [Decl] -> [(String, Type)]
flattenDecls decls = concat $ fmap (\ (Dec idents typ) -> nameType typ idents ) decls
    where nameType typ = fmap (\ (Ident name) -> (name, typ) )

flatten :: [Ident] -> StoreVal -> [(String,StoreVal)]
flatten idents val = fmap (\ (Ident name) -> (name, val) ) idents

toIdsAndVals :: [Decl] -> Execution [ [(String, StoreVal)] ]
toIdsAndVals decls = 
    sequence $ fmap (\ (Dec idents typ) -> liftM (flatten idents) (zero typ) ) decls

zeros :: [Decl] -> Execution [(String,StoreVal)]
zeros decls = liftM concat (toIdsAndVals decls)

defineDecl :: Decl -> Execution () -> Execution ()
defineDecl (Dec idents typ) exec = do
    --initialise idents
    val <- zero typ
    let vals = fmap (\_ -> val) idents
        names = fmap (\ (Ident name) -> name ) idents
    locs <- sequence $ fmap alloc vals
    let envLocs = fmap EnvLoc locs
    local (insertMulti (zip names envLocs)) exec
    dealloc locs

insertMulti :: Ord k => [(k,v)] -> Map.Map k v -> Map.Map k v
insertMulti l m = foldr (\(k,v) acc -> Map.insert k v acc) m l
