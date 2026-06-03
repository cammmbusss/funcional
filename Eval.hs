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
evalExp = undefined
