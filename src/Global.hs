module Global(
  GameState,
  playerInCheck,
  wonGame,
  moveCanPromote,
  currentBoard,
  currentGame,
  currentPlayer,
  currentTakenPieces,
  initialState
) where

import Common

data GameState = GameState {
  playerInCheck :: Bool,        -- True, si el jugador esta en jaque, se le limitar los movimientos
  wonGame :: Bool,              -- True, si el jugador actual hizo un jaque mate
  moveCanPromote :: Bool,   -- True, si el ultimo movimiento dejo una pieza promovible 
  currentBoard :: Board,        -- Muestra el tablero actual
  currentGame :: Game,          -- Cada jugada se almacena aqui
  currentPlayer :: Player,      -- Jugador actual
  currentTakenPieces :: (TakenPieces,TakenPieces) -- Dos listas con las piezas que alguno de los jugadores puede poner
}

-- Estado inicial
initialState :: GameState
initialState = GameState False False False boardInit [] Black ([],[])