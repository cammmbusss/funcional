{- TAREA DE PROGRAMACIÓN FUNCIONAL 2026 -}
{- PRETTY-PRINTING -}
module PP where

import AST

-- Pretty-printing de un programa
-- El comportamiento de la función se especifica en la letra de la Tarea.

ppProg :: Prog -> String
ppProg prog = concatMap (ppFun 0) prog 


ppFun :: Int -> Fun -> String
ppFun i (Fun name arg stmts ret) =  
    indent i ++ "fun " ++ name ++ " " ++ arg ++ " {\n" ++
    ppStmts (i+1) stmts ++
    indent i ++ "} " ++ ppExp ret ++ ";\n"


ppStmts :: Int -> Stmts -> String 
ppStmts i ss = concatMap (ppStmt i) ss 


ppStmt :: Int -> Stmt -> String
ppStmt i stmt = case stmt of

    Assign x e ->
        indent i ++ x ++ " := " ++ ppExp e ++ ";\n"

    While cond body ->
        indent i ++ "while " ++ ppExp cond ++ " {\n" ++
        ppStmts (i+1) body ++
        indent i ++ "}\n"

    If cond th el ->
        indent i ++ "if " ++ ppExp cond ++ " {\n" ++
        ppStmts (i+1) th ++
        indent i ++ "} else {\n" ++
        ppStmts (i+1) el ++
        indent i ++ "}\n"

    Case e clauses ->
        indent i ++ "case " ++ ppExp e ++ " {\n" ++
        concatMap (ppClause (i+1)) clauses ++
        indent i ++ "}\n"



ppClause :: Int -> Clause -> String
ppClause i (Clause pat stmts) =
    indent i ++ ppPattern pat ++ " {\n" ++
    ppStmts (i+1) stmts ++
    indent i ++ "}\n"


ppPattern :: Pattern -> String
ppPattern pat = case pat of
    PNil -> "Nil"
    PCons p1 p2 -> "(Cons " ++ ppPattern p1 ++ " " ++ ppPattern p2 ++ ")"
    PLitN n -> show n
    PLitB b -> show b
    PVar x -> x



ppExp :: Exp -> String
ppExp expr = case expr of

    LitN n -> show n
    LitB b -> show b
    Var x -> x
    Nil -> "Nil"

    Cons e1 e2 ->
        "(Cons " ++ ppExp e1 ++ " " ++ ppExp e2 ++ ")"

    Head e ->
        "(head " ++ ppExp e ++ ")"

    Tail e ->
        "(tail " ++ ppExp e ++ ")"

    Call f e ->
        "(" ++ f ++ " " ++ ppExp e ++ ")"

    BinOp op e1 e2 ->
        "(" ++ ppExp e1 ++ " " ++ ppOp op ++ " " ++ ppExp e2 ++ ")"

    UnOp op e ->
        "(" ++ ppUOp op ++ " " ++ ppExp e ++ ")"


ppOp :: BOp -> String
ppOp op = case op of
    Add   -> "+"
    Sub   -> "-"
    Times -> "*"
    Div   -> "/"
    Mod   -> "%"
    And   -> "&&"
    Or    -> "||"
    Equ   -> "=="
    Lt    -> "<"


ppUOp :: UOp -> String
ppUOp op = case op of
    Minus -> "-"
    Not   -> "!"


indent :: Int -> String
indent i = replicate (i * 4) ' '