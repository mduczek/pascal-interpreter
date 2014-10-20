module Interpreter (runInterpretation) where

{-
Applying ErrorT to the State monad gives a state transformer function of 
type s -> (Error e a,s), thus an error means no value could be produced, 
but the state remains valid.
-}
import System.IO ( stderr, hPutStrLn)
import Execution
import Declarations
import Statements
import AbsVar
import Builtin
import Control.Monad.Reader


tree = Prog (Ident "exFunction") (RecBlockJust [Rec (Ident "person") [Dec [Ident "a"] (TArray TBool),Dec [Ident "b"] TBool]]) (DecBlockJust [Dec [Ident "p"] (TRecord (Ident "person")),Dec [Ident "c"] TBool]) [] [SExp (EAss (AVar (VarSimple (Ident "c"))) (Eeq (EAss (AVar (VarFieldAccess (VarSimple (Ident "p")) (Ident "b"))) Etrue) Etrue))]

interpret :: Program -> Execution ()
interpret (Prog ident recBlock decBlock functions stms) = do
    defineBuiltin builtinFuns $ defineRecBlock recBlock $ defineDecBlock decBlock $ defineFuns functions $ run (SComp stms)


runInterpretation tree = do
    (lr, stack) <- runExecution (interpret tree) 2
    case lr of
        (Left msg) -> hPutStrLn stderr msg
        (Right _) -> return ()