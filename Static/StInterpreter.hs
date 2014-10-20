module StInterpreter (runInterpretation) where

{-
Applying ErrorT to the State monad gives a state transformer function of 
type s -> (Error e a,s), thus an error means no value could be produced, 
but the state remains valid.
-}

import StExecution
import StDeclarations
import StStatements
import AbsVar
import StBuiltin

tree = Prog (Ident "exFunction") RecBlockEmpty (DecBlockJust [Dec [Ident "a"] (TArray TInt)]) [] [SExp (Call (Ident "init") [EVar (VarSimple (Ident "a")),EInt 10]),SExp (Call (Ident "print") [EVar (VarSimple (Ident "a"))])]

interpret :: Program -> Execution ()
interpret (Prog ident recBlock decBlock functions stms) = do
    defineFuns builtinFuns $ defineRecBlock recBlock $ defineDecBlock decBlock $ defineFuns functions $ run (SComp stms)
    --run (SComp [SExp Etrue])


runInterpretation tree = runExecution (interpret tree) 2

--main :: IO ()
--main = do
--    a <- runExecution alloc 2
--    putStr $ show a