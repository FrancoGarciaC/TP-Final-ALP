module Global(
  GameState,
  gameInSession,
  replayInSession,
  currentBoard,
  currentGame,
  currentPlayer,
  currentTakenPieces,
  initialState
) where

import Common

data GameState = GameState {
  gameInSession :: Bool,        --  True, si hay una partida en juego.
  replayInSession :: Bool,      -- True, si se esta reproduciendo un replay.
  currentBoard :: Board,        -- Muestra el tablero actual
  currentGame :: Game,          -- Cada jugada se almacena aqui
  currentPlayer :: Player,      -- Jugador actual
  currentTakenPieces :: (TakenPieces,TakenPieces) -- Dos listas con las piezas que alguno de los jugadores puede poner
}

-- Estado inicial
initialState :: GameState
initialState = GameState False False boardInit [] Black ([],[])