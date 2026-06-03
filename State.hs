{- TAREA DE PROGRAMACIÓN FUNCIONAL 2026 -}
{- ESTADO DE LAS VARIABLES -}
module State where

import AST

-- Estado
type State = [Frame]

type Frame = [(String, Val)]

-- Obtiene el valor de una variable en el estado dado su identificador
get :: Id -> State -> Maybe Val
get id
  = foldr upd Nothing
  where
    upd frame acc = case lookup id frame of
        Just val -> Just val
        Nothing -> acc

-- Descarta un frame del estado
dropFrame :: State -> State
dropFrame  = drop 1

-- Construye un nuevo frame en el estado
newFrame :: State -> State
newFrame env = [] : env

-- Agrega una nueva variable a un frame del estado
new :: Id -> Val -> State -> State
new id val (f:fs) = ((id, val) : f) : fs


-- Actualiza el valor de una variable en un frame del estado
updateFrame :: Id -> Val -> Frame -> Frame
updateFrame id val
  = map (\(k,v) -> if k == id then (k,val) else (k,v))

-- Actualiza el valor de una variable en el estado
-- Si la variable no existe, se agrega al frame superior
set :: Id -> Val -> State -> State
set id val (f:fs) =
  case setInFirstFrame id val f of
    (f', True)  -> f' : fs
    (f', False) ->
      case setInRest id val fs of
        Just fs' -> f : fs'
        Nothing  -> updateTop f
  where
    setInFirstFrame :: Id -> Val -> Frame -> (Frame, Bool)
    setInFirstFrame i v fr =
      let exists = lookup i fr /= Nothing
          fr'     = updateFrame i v fr
      in (fr', exists)

    setInRest :: Id -> Val -> [Frame] -> Maybe [Frame]
    setInRest _ _ [] = Nothing
    setInRest i v (g:gs)
      | lookup i g /= Nothing = Just (updateFrame i v g : gs)
      | otherwise =
          case setInRest i v gs of
            Just gs' -> Just (g : gs')
            Nothing  -> Nothing

    updateTop :: Frame -> State
    updateTop fr = ((id, val) : filter ((/= id) . fst) fr) : fs
