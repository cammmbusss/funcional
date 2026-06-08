{- TAREA DE PROGRAMACIÓN FUNCIONAL 2026 -}
{- CHEQUEO DE NOMBRES Y TIPOS -}
module TypeChecker where

import AST

-- Tipos
data Type = TInt | TBool | TList
  deriving (Eq)

-- Resultado de un Chequeo
data CheckRes = Ok
              | HasNameErrors [NameError]
              | HasTypeErrors [TypeError]

-- Errores de Nombres
data NameError
  = UndefVar Id
  | UndefFun Id
  | DupFun Id
  | DupVar Id

-- Errores de Tipos
data TypeError
  = CallArgType Id Type
  | BinOpWrongType BOp Type Type
  | UnOpWrongType UOp Type
  | CondNotBool Type
  | AssignTypeMismatch Id Type Type
  | PatMismatch Type Type
  | ConsExpType Type Type
  | HeadTailArg Type
  | WrongReturnType Id Type


-- Instancias de Show de tipos y resultados
instance Show Type where
  show TInt = "int"
  show TBool = "bool"
  show TList = "list"

instance Show NameError where
  showsPrec _ err = case err of
    UndefVar x ->
      showString "undefined variable: " . showString x

    UndefFun f ->
      showString "undefined function: " . showString f

    DupFun f ->
      showString "duplicated function: " . showString f

    DupVar v ->
      showString "duplicated variable: " . showString v

instance Show TypeError where
  showsPrec _ err = case err of
    CallArgType f t ->
      showString "invalid argument type in "
      . showString f
      . showString ": "
      . shows t

    BinOpWrongType bop t1 t2 ->
      showString "invalid argument type/s in operator "
      . shows bop
      . showString ": "
      . shows t1
      . showString ", "
      . shows t2

    UnOpWrongType uop t ->
      showString "invalid argument type in unary operator "
      . shows uop
      . showString ": "
      . shows t

    CondNotBool t ->
      showString "invalid condition type: "
      . shows t

    AssignTypeMismatch x t1 t2 ->
      showString "invalid assignment in "
      . showString x
      . showString ": expected "
      . shows t1
      . showString ", actual "
      . shows t2

    PatMismatch t1 t2 ->
      showString "invalid pattern: expected "
      . shows t1
      . showString ", actual "
      . shows t2

    ConsExpType t1 t2 ->
      showString "invalid argument type/s in Cons: "
      . shows t1
      . showString ", "
      . shows t2

    HeadTailArg t ->
      showString "invalid list argument type: "
      . shows t

    WrongReturnType f t ->
      showString "invalid return type in "
      . showString f
      . showString ": "
      . shows t

instance Show CheckRes where
  showsPrec _ Ok = showString "ok"
  showsPrec _ (HasNameErrors errs) = showLines errs
  showsPrec _ (HasTypeErrors errs) = showLines errs

showLines :: Show a => [a] -> ShowS
showLines =
  foldr1 (\x acc -> x . showChar '\n' . acc) . map shows


-- Chequeo de un programa.
-- El comportamiento de la función se especifica en la letra de la Tarea.
checkProg :: Prog -> CheckRes
checkProg prog =
  let nameErrs = checkNames prog
  in if not (null nameErrs)
     then HasNameErrors nameErrs
     else
       let typeErrs = checkTypes prog
       in if null typeErrs then Ok else HasTypeErrors typeErrs


-- Chequeo de una expresión.
-- El comportamiento de la función se especifica en la letra de la Tarea.
checkExp :: Prog -> Exp -> CheckRes
checkExp prog exp =
  case checkProg prog of
    HasNameErrors errs -> HasNameErrors errs
    _ ->
      let (_, errs) = checkExpType exp []
      in if null errs then Ok else HasTypeErrors errs



type Env = [Id]

checkNames :: Prog -> [NameError]
checkNames funs =
  checkDupFuns funs ++ checkFuns funs []

checkDupFuns :: Prog -> [NameError]
checkDupFuns [] = []
checkDupFuns (Fun f _ _ _ : fs) =
  if any (\(Fun g _ _ _) -> f == g) fs
  then DupFun f : checkDupFuns fs
  else checkDupFuns fs

checkFuns :: Prog -> [Id] -> [NameError]
checkFuns [] _ = []
checkFuns (Fun f param stmts ret : fs) funEnv =
  let vars = [param]
      errs = checkStmts stmts vars (f:funEnv)
             ++ checkExpNames ret vars (f:funEnv)
  in errs ++ checkFuns fs (f:funEnv)

checkStmts :: Stmts -> Env -> [Id] -> [NameError]
checkStmts [] _ _ = []
checkStmts (s:ss) vars funs =
  case s of
    Assign x e ->
      checkExpNames e vars funs
      ++ checkStmts ss (x:vars) funs

    While e body ->
      checkExpNames e vars funs
      ++ checkStmts body vars funs
      ++ checkStmts ss vars funs

    If e t f ->
      checkExpNames e vars funs
      ++ checkStmts t vars funs
      ++ checkStmts f vars funs
      ++ checkStmts ss vars funs

    Case e clauses ->
      checkExpNames e vars funs
      ++ concatMap (checkClause vars funs) clauses
      ++ checkStmts ss vars funs

checkClause :: Env -> [Id] -> Clause -> [NameError]
checkClause vars funs (Clause pat stmts) =
  let (newVars, errs) = checkPattern pat vars
  in errs ++ checkStmts stmts (newVars ++ vars) funs

checkPattern :: Pattern -> Env -> (Env, [NameError])
checkPattern pat vars =
  case pat of
    PVar x ->
      if x `elem` vars then ([], [DupVar x]) else ([x], [])

    PCons p1 p2 ->
      let (v1, e1) = checkPattern p1 vars
          (v2, e2) = checkPattern p2 (v1 ++ vars)
      in (v1 ++ v2, e1 ++ e2)

    _ -> ([], [])

checkExpNames :: Exp -> Env -> [Id] -> [NameError]
checkExpNames exp vars funs =
  case exp of
    Var x ->
      if x `elem` vars then [] else [UndefVar x]

    Call f e ->
      let fErr = if f `elem` funs then [] else [UndefFun f]
      in fErr ++ checkExpNames e vars funs

    BinOp _ e1 e2 ->
      checkExpNames e1 vars funs ++ checkExpNames e2 vars funs

    UnOp _ e ->
      checkExpNames e vars funs

    Cons e1 e2 ->
      checkExpNames e1 vars funs ++ checkExpNames e2 vars funs

    Head e -> checkExpNames e vars funs
    Tail e -> checkExpNames e vars funs

    _ -> []


type TEnv = [(Id, Type)]

checkTypes :: Prog -> [TypeError]
checkTypes = concatMap checkFun

checkFun :: Fun -> [TypeError]
checkFun (Fun f param stmts ret) =
  let env = [(param, TList)]
      (env2, errs1) = checkStmtsType stmts env
      (t, errs2) = checkExpType ret env2
  in errs1 ++ errs2 ++ if t /= TList then [WrongReturnType f t] else []

checkStmtsType :: Stmts -> TEnv -> (TEnv, [TypeError])
checkStmtsType [] env = (env, [])
checkStmtsType (s:ss) env =
  case s of
    Assign x e ->
      let (t, errs1) = checkExpType e env
      in case lookup x env of
        Nothing ->
          let env' = (x,t):env
              (env2, errs2) = checkStmtsType ss env'
          in (env2, errs1 ++ errs2)

        Just tOld ->
          let err = if tOld /= t then [AssignTypeMismatch x tOld t] else []
              (env2, errs2) = checkStmtsType ss env
          in (env2, errs1 ++ err ++ errs2)

    While e body ->
      let (t, errs1) = checkExpType e env
          err = if t /= TBool then [CondNotBool t] else []
          (_, errsBody) = checkStmtsType body env
          (env2, errs2) = checkStmtsType ss env
      in (env2, errs1 ++ err ++ errsBody ++ errs2)

    If e t f ->
      let (tc, ec) = checkExpType e env
          err = if tc /= TBool then [CondNotBool tc] else []
          (_, et) = checkStmtsType t env
          (_, ef) = checkStmtsType f env
          (env2, errs2) = checkStmtsType ss env
      in (env2, ec ++ err ++ et ++ ef ++ errs2)

    Case e clauses ->
      let (t, errs1) = checkExpType e env
          errsClauses = concatMap (checkClauseType t env) clauses
          (env2, errs2) = checkStmtsType ss env
      in (env2, errs1 ++ errsClauses ++ errs2)

checkClauseType :: Type -> TEnv -> Clause -> [TypeError]
checkClauseType t env (Clause pat stmts) =
  let (envPat, tPat, errs1) = checkPatternType pat env
      err = if t /= tPat then [PatMismatch t tPat] else []
      (_, errs2) = checkStmtsType stmts (envPat ++ env)
  in errs1 ++ err ++ errs2

checkPatternType :: Pattern -> TEnv -> (TEnv, Type, [TypeError])
checkPatternType pat env =
  case pat of
    PNil -> ([], TList, [])
    PLitN _ -> ([], TInt, [])
    PLitB _ -> ([], TBool, [])

    PVar x -> ([(x, TInt)], TInt, [])

    PCons p1 p2 ->
      let (e1, t1, er1) = checkPatternType p1 env
          (e2, t2, er2) = checkPatternType p2 env
          err = if t1 /= TInt || t2 /= TList then [ConsExpType t1 t2] else []
      in (e1 ++ e2, TList, er1 ++ er2 ++ err)


-- Exp type
checkExpType :: Exp -> TEnv -> (Type, [TypeError])
checkExpType exp env =
  case exp of
    LitN _ -> (TInt, [])
    LitB _ -> (TBool, [])

    Var x ->
      case lookup x env of
        Just t -> (t, [])
        Nothing -> (TInt, [])

    Nil -> (TList, [])

    Cons e1 e2 ->
      let (t1, e1e) = checkExpType e1 env
          (t2, e2e) = checkExpType e2 env
          err = if t1 == TInt && t2 == TList then [] else [ConsExpType t1 t2]
      in (TList, e1e ++ e2e ++ err)

    Head e ->
      let (t, errs) = checkExpType e env
      in if t == TList then (TInt, errs) else (TInt, errs ++ [HeadTailArg t])

    Tail e ->
      let (t, errs) = checkExpType e env
      in if t == TList then (TList, errs) else (TList, errs ++ [HeadTailArg t])

    BinOp op e1 e2 ->
      let (t1, e1e) = checkExpType e1 env
          (t2, e2e) = checkExpType e2 env
          errs = e1e ++ e2e
      in case op of
        Add -> checkIntOp op t1 t2 errs
        Sub -> checkIntOp op t1 t2 errs
        Times -> checkIntOp op t1 t2 errs
        Div -> checkIntOp op t1 t2 errs
        Mod -> checkIntOp op t1 t2 errs
        And -> checkBoolOp op t1 t2 errs
        Or  -> checkBoolOp op t1 t2 errs
        Equ ->
          if t1 == t2 then (TBool, errs)
          else (TBool, errs ++ [BinOpWrongType op t1 t2])
        Lt ->
          if t1 == TInt && t2 == TInt then (TBool, errs)
          else (TBool, errs ++ [BinOpWrongType op t1 t2])

    UnOp op e ->
      let (t, errs) = checkExpType e env
      in case op of
        Minus ->
          if t == TInt then (TInt, errs)
          else (TInt, errs ++ [UnOpWrongType op t])
        Not ->
          if t == TBool then (TBool, errs)
          else (TBool, errs ++ [UnOpWrongType op t])

    Call f e ->
      let (t, errs) = checkExpType e env
          err = if t == TList then [] else [CallArgType f t]
      in (TList, errs ++ err)


--helpers

checkIntOp :: BOp -> Type -> Type -> [TypeError] -> (Type, [TypeError])
checkIntOp op t1 t2 errs =
  if t1 == TInt && t2 == TInt
  then (TInt, errs)
  else (TInt, errs ++ [BinOpWrongType op t1 t2])

checkBoolOp :: BOp -> Type -> Type -> [TypeError] -> (Type, [TypeError])
checkBoolOp op t1 t2 errs =
  if t1 == TBool && t2 == TBool
  then (TBool, errs)
  else (TBool, errs ++ [BinOpWrongType op t1 t2])