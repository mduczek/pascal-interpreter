{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintVar where

-- pretty-printer generated by the BNF converter

import AbsVar
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: [a] -> Doc
  prtList = concatD . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))
  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])



instance Print Program where
  prt i e = case e of
   Prog id recblock decblock functions stms -> prPrec i 0 (concatD [doc (showString "program") , prt 0 id , doc (showString ";") , prt 0 recblock , prt 0 decblock , prt 0 functions , doc (showString "begin") , prt 0 stms , doc (showString "end") , doc (showString ".")])


instance Print Function where
  prt i e = case e of
   Fun id decls returntype decblock functions stms -> prPrec i 0 (concatD [doc (showString "function") , prt 0 id , doc (showString "(") , prt 0 decls , doc (showString ")") , doc (showString ":") , prt 0 returntype , doc (showString ";") , prt 0 decblock , prt 0 functions , doc (showString "begin") , prt 0 stms , doc (showString "end") , doc (showString ";")])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print RecBlock where
  prt i e = case e of
   RecBlockJust records -> prPrec i 0 (concatD [doc (showString "type") , prt 0 records])
   RecBlockEmpty  -> prPrec i 0 (concatD [])


instance Print Record where
  prt i e = case e of
   Rec id decls -> prPrec i 0 (concatD [prt 0 id , doc (showString "=") , doc (showString "record") , prt 0 decls , doc (showString "end") , doc (showString ";")])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print DecBlock where
  prt i e = case e of
   DecBlockJust decls -> prPrec i 0 (concatD [doc (showString "var") , prt 0 decls])
   DecBlockEmpty  -> prPrec i 0 (concatD [])


instance Print Decl where
  prt i e = case e of
   Dec ids type' -> prPrec i 0 (concatD [prt 0 ids , doc (showString ":") , prt 0 type'])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ";") , prt 0 xs])

instance Print Var where
  prt i e = case e of
   VarSimple id -> prPrec i 0 (concatD [prt 0 id])
   VarIndexed var exp -> prPrec i 0 (concatD [prt 0 var , doc (showString "[") , prt 0 exp , doc (showString "]")])
   VarFieldAccess var id -> prPrec i 0 (concatD [prt 0 var , doc (showString ".") , prt 0 id])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ";") , prt 0 xs])

instance Print Stm where
  prt i e = case e of
   SComp stms -> prPrec i 0 (concatD [doc (showString "begin") , prt 0 stms , doc (showString "end") , doc (showString ";")])
   SExp exp -> prPrec i 0 (concatD [prt 0 exp , doc (showString ";")])
   SWhile exp stm -> prPrec i 0 (concatD [doc (showString "while") , prt 0 exp , doc (showString "do") , prt 0 stm])
   SFor exp0 exp1 exp stm -> prPrec i 0 (concatD [doc (showString "for") , prt 0 exp0 , doc (showString ";") , prt 0 exp1 , doc (showString ";") , prt 0 exp , doc (showString "do") , prt 0 stm])
   SIt exp stm elifs -> prPrec i 0 (concatD [doc (showString "if") , prt 0 exp , doc (showString "then") , prt 0 stm , prt 0 elifs , doc (showString "endif")])
   SIte exp stm0 elifs stm -> prPrec i 0 (concatD [doc (showString "if") , prt 0 exp , doc (showString "then") , prt 0 stm0 , prt 0 elifs , doc (showString "else") , prt 0 stm , doc (showString "endif")])
   SLength id exp -> prPrec i 0 (concatD [prt 0 id , doc (showString "length") , prt 0 exp , doc (showString ";")])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Elif where
  prt i e = case e of
   Eliff exp stm -> prPrec i 0 (concatD [doc (showString "elif") , prt 0 exp , doc (showString "then") , prt 0 stm])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print ReturnType where
  prt i e = case e of
   RetType type' -> prPrec i 0 (concatD [prt 0 type'])
   Void  -> prPrec i 0 (concatD [doc (showString "void")])


instance Print Type where
  prt i e = case e of
   TTuple type' types -> prPrec i 0 (concatD [doc (showString "(") , prt 0 type' , doc (showString ",") , prt 0 types , doc (showString ")")])
   TBool  -> prPrec i 0 (concatD [doc (showString "bool")])
   TInt  -> prPrec i 0 (concatD [doc (showString "int")])
   TString  -> prPrec i 0 (concatD [doc (showString "string")])
   TArray type' -> prPrec i 0 (concatD [doc (showString "array") , doc (showString "<") , prt 0 type' , doc (showString ">")])
   TMap type'0 type' -> prPrec i 0 (concatD [doc (showString "map") , doc (showString "<") , prt 0 type'0 , doc (showString ",") , prt 0 type' , doc (showString ">")])
   TRecord id -> prPrec i 0 (concatD [prt 0 id])
   TWildcard  -> prPrec i 0 (concatD [doc (showString "?")])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Assignable where
  prt i e = case e of
   ATuple var vars -> prPrec i 0 (concatD [doc (showString "(") , prt 0 var , doc (showString ";") , prt 0 vars , doc (showString ")")])
   AVar var -> prPrec i 0 (concatD [prt 0 var])


instance Print Exp where
  prt i e = case e of
   EAss assignable exp -> prPrec i 0 (concatD [prt 0 assignable , doc (showString "=") , prt 0 exp])
   Eor exp0 exp -> prPrec i 1 (concatD [prt 1 exp0 , doc (showString "or") , prt 2 exp])
   Eand exp0 exp -> prPrec i 2 (concatD [prt 2 exp0 , doc (showString "and") , prt 3 exp])
   Eeq exp0 exp -> prPrec i 3 (concatD [prt 3 exp0 , doc (showString "==") , prt 4 exp])
   Eneq exp0 exp -> prPrec i 3 (concatD [prt 3 exp0 , doc (showString "<>") , prt 4 exp])
   Elt exp0 exp -> prPrec i 4 (concatD [prt 4 exp0 , doc (showString "<") , prt 5 exp])
   Elte exp0 exp -> prPrec i 4 (concatD [prt 4 exp0 , doc (showString "<=") , prt 5 exp])
   Egt exp0 exp -> prPrec i 4 (concatD [prt 4 exp0 , doc (showString ">") , prt 5 exp])
   Egte exp0 exp -> prPrec i 4 (concatD [prt 4 exp0 , doc (showString ">=") , prt 5 exp])
   EConcat exp0 exp -> prPrec i 5 (concatD [prt 5 exp0 , doc (showString "++") , prt 6 exp])
   EAdd exp0 exp -> prPrec i 5 (concatD [prt 5 exp0 , doc (showString "+") , prt 6 exp])
   ESub exp0 exp -> prPrec i 5 (concatD [prt 5 exp0 , doc (showString "-") , prt 6 exp])
   EMul exp0 exp -> prPrec i 6 (concatD [prt 6 exp0 , doc (showString "*") , prt 7 exp])
   EDiv exp0 exp -> prPrec i 6 (concatD [prt 6 exp0 , doc (showString "/") , prt 7 exp])
   Call id exps -> prPrec i 7 (concatD [prt 0 id , doc (showString "(") , prt 0 exps , doc (showString ")")])
   ETuple exp exps -> prPrec i 7 (concatD [doc (showString "(") , prt 0 exp , doc (showString ",") , prt 0 exps , doc (showString ")")])
   EBNeg exp -> prPrec i 7 (concatD [doc (showString "!") , prt 8 exp])
   EMinus exp -> prPrec i 7 (concatD [doc (showString "-") , prt 8 exp])
   ECast exp type' -> prPrec i 7 (concatD [doc (showString "say") , prt 0 exp , doc (showString "is") , prt 0 type'])
   EVar var -> prPrec i 8 (concatD [prt 0 var])
   EInt n -> prPrec i 8 (concatD [prt 0 n])
   Etrue  -> prPrec i 8 (concatD [doc (showString "true")])
   Efalse  -> prPrec i 8 (concatD [doc (showString "false")])
   EStr str -> prPrec i 8 (concatD [prt 0 str])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

