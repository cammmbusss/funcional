{- TAREA DE PROGRAMACIÓN FUNCIONAL 2026 -}
{- EVALUACIÓN DE EXPRESIONES -}
module Eval where

import AST
import State


-- Resultado de una evaluación
type EvalRes = Either RuntimeError Val

-- Errores en tiempo de ejecución
data RuntimeError
  = HeadOfEmptyList
  | TailOfEmptyList
  | DivisionByZero
  deriving Eq

instance Show RuntimeError where
  show HeadOfEmptyList = "head of empty list"
  show TailOfEmptyList = "tail of empty list"
  show DivisionByZero  = "division by zero"

  
-- Evalúa una expresión.
-- El comportamiento de la función se especifica en la letra de la Tarea.
evalExp :: Prog -> State -> Exp -> EvalRes

evalExp _ _ (LitN n) = Right (ValInt n)
evalExp _ _ (LitB b) = Right (ValBool b)

evalExp _ st (Var x) =
  case get x st of
    Just v  -> Right v
    Nothing -> error ("Variable no definida: " ++ x)

evalExp _ _ Nil = Right (ValList [])

evalExp p st (Cons e1 e2) = do
  v1 <- evalExp p st e1
  v2 <- evalExp p st e2
  case (v1, v2) of
    (ValInt n, ValList xs) -> Right (ValList (n:xs))
    _ -> error "Type error en Cons"

evalExp p st (Head e) = do
  v <- evalExp p st e
  case v of
    ValList (x:_) -> Right (ValInt x)
    ValList []    -> Left HeadOfEmptyList
    _             -> error "Type error en Head"

evalExp p st (Tail e) = do
  v <- evalExp p st e
  case v of
    ValList (_:xs) -> Right (ValList xs)
    ValList []     -> Left TailOfEmptyList
    _              -> error "Type error en Tail"

evalExp p st (BinOp op e1 e2) = do
  v1 <- evalExp p st e1
  v2 <- evalExp p st e2
  evalBinOp op v1 v2

evalExp p st (UnOp op e) = do
  v <- evalExp p st e
  evalUnOp op v

-- =========================
-- CALL
-- =========================

evalExp p st (Call f e) = do
  arg <- evalExp p st e
  case lookupFun f p of
    Nothing -> error ("Función no definida: " ++ f)
    Just (Fun _ param stmts retExp) -> do
      let st1 = newFrame []
      let st2 = new param arg st1
      stFinal <- execStmts p st2 stmts
      let stOut = dropFrame stFinal
      evalExp p stOut retExp


-- =========================
-- STMTS
-- =========================

execStmts :: Prog -> State -> Stmts -> Either RuntimeError State
execStmts _ st [] = Right st
execStmts p st (s:ss) = do
  st' <- execStmt p st s
  execStmts p st' ss


execStmt :: Prog -> State -> Stmt -> Either RuntimeError State

execStmt p st (Assign x e) = do
  v <- evalExp p st e
  return (set x v st)

execStmt p st (While cond body) = do
  v <- evalExp p st cond
  case v of
    ValBool True -> do
      st' <- execStmts p st body
      execStmt p st' (While cond body)
    ValBool False -> Right st
    _ -> error "While espera Bool"

execStmt p st (If cond tbranch fbranch) = do
  v <- evalExp p st cond
  case v of
    ValBool True  -> execStmts p st tbranch
    ValBool False -> execStmts p st fbranch
    _ -> error "If espera Bool"

execStmt p st (Case e clauses) = do
  v <- evalExp p st e
  matchClauses p st v clauses


-- =========================
-- CASE
-- =========================

matchClauses :: Prog -> State -> Val -> [Clause] -> Either RuntimeError State
matchClauses _ _ _ [] = error "No hay match en case"
matchClauses p st v (Clause pat stmts : cs) =
  case matchPattern pat v of
    Nothing -> matchClauses p st v cs
    Just bindings ->
      let st' = foldr (\(x,val) acc -> new x val acc) (newFrame st) bindings
      in do
        stFinal <- execStmts p st' stmts
        return (dropFrame stFinal)


matchPattern :: Pattern -> Val -> Maybe [(Id, Val)]

matchPattern PNil (ValList []) = Just []

matchPattern (PLitN n) (ValInt m)
  | n == m = Just []

matchPattern (PLitB b) (ValBool c)
  | b == c = Just []

matchPattern (PVar x) v = Just [(x,v)]

matchPattern (PCons p1 p2) (ValList (x:xs)) = do
  b1 <- matchPattern p1 (ValInt x)
  b2 <- matchPattern p2 (ValList xs)
  return (b1 ++ b2)

matchPattern _ _ = Nothing


-- =========================
-- OPS
-- =========================

evalBinOp :: BOp -> Val -> Val -> EvalRes

evalBinOp Add (ValInt a) (ValInt b) = Right (ValInt (a + b))
evalBinOp Sub (ValInt a) (ValInt b) = Right (ValInt (a - b))
evalBinOp Times (ValInt a) (ValInt b) = Right (ValInt (a * b))

evalBinOp Div (ValInt _) (ValInt 0) = Left DivisionByZero
evalBinOp Div (ValInt a) (ValInt b) = Right (ValInt (a `div` b))

evalBinOp Mod (ValInt _) (ValInt 0) = Left DivisionByZero
evalBinOp Mod (ValInt a) (ValInt b) = Right (ValInt (a `mod` b))

evalBinOp And (ValBool a) (ValBool b) = Right (ValBool (a && b))
evalBinOp Or  (ValBool a) (ValBool b) = Right (ValBool (a || b))

evalBinOp Equ v1 v2 = Right (ValBool (v1 == v2))

evalBinOp Lt (ValInt a) (ValInt b) = Right (ValBool (a < b))

evalBinOp _ _ _ = error "Error de tipos en BinOp"


evalUnOp :: UOp -> Val -> EvalRes

evalUnOp Minus (ValInt n) = Right (ValInt (-n))
evalUnOp Not (ValBool b) = Right (ValBool (not b))
evalUnOp _ _ = error "Error en UnOp"


-- =========================
-- FUN LOOKUP
-- =========================

lookupFun :: Id -> Prog -> Maybe Fun
lookupFun _ [] = Nothing
lookupFun f (fn@(Fun name _ _ _):fs)
  | f == name = Just fn
  | otherwise = lookupFun f fs