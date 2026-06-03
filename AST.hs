{- TAREA DE PROGRAMACIÓN FUNCIONAL 2026 -}
{- ÁRBOL DE SINTAXIS ABSTRACTA -}

module AST where

type Id = String

-- Programa
type Prog = [Fun]

-- Funciones
data Fun = Fun Id Id Stmts Exp deriving Show

-- Instrucciones
type Stmts = [Stmt]

data Stmt
  = Assign Id Exp
  | While Exp Stmts
  | If Exp Stmts Stmts
  | Case Exp [Clause]
  deriving Show

-- Pattern matching
data Clause
  = Clause Pattern Stmts
  deriving Show

type PId = String

data Pattern
  = PNil
  | PCons Pattern Pattern
  | PLitN Integer
  | PLitB Bool
  | PVar PId
  deriving Show

-- Expresiones
data Exp
  = LitN Integer
  | LitB Bool
  | Cons Exp Exp
  | Nil
  | Head Exp
  | Tail Exp
  | Call Id Exp
  | Var Id
  | BinOp BOp Exp Exp
  | UnOp UOp Exp
  deriving Show

-- Operadores
data BOp = Add | Sub | Times | Div | Mod | And | Or | Equ | Lt
  deriving Show

data UOp = Minus | Not
  deriving Show


-- Valores
data Val
  = ValInt Integer
  | ValBool Bool
  | ValList [Integer]
  deriving Eq


-- Instancia de Show para los valores
instance Show Val where
  showsPrec _ (ValInt n)   = shows n
  showsPrec _ (ValBool b)  = shows b
  showsPrec _ (ValList xs) = showsListP False xs
    where
      showsListP _ []     = showString "Nil"
      showsListP p (x:xs) = showParen p $ showString "Cons " . shows x . showString " " . showsListP True xs            
