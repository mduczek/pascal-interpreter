module StBuiltin (builtinFuns) where

import AbsVar

--arrayTypeCheck = SExp (EAss (AVar (VarIndexed (VarSimple (Ident "array")) (EInt 0))) (VarSimple (Ident "a")))

fprint = Fun (Ident "print") [Dec [Ident "a"] TWildcard ] Void DecBlockEmpty [] []
str = Fun (Ident "str") [Dec [Ident "a"] TWildcard ] (RetType TString) DecBlockEmpty [] []
parseStr = Fun (Ident "str_to_num") [Dec [Ident "a"] TString ] (RetType TInt) DecBlockEmpty [] []
len = Fun (Ident "len") [Dec [Ident "array"] (TArray TWildcard)] (RetType TInt) DecBlockEmpty [] []


builtinFuns = [fprint, len, str, parseStr]