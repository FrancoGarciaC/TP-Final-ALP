{-# LANGUAGE OverloadedStrings #-}

module Common where

import Data.Matrix
--Terminos:
-- This represents every posible piece in a Shogi board
data Piece = King
            | Rook 
            | Bishop 
            | Golden 
            | Silver
            | Knight 
            | Lancer 
            | Pawn 
            | Dragon 
            | Horse 
            | PromotedSilver 
            | PromotedKnight 
            | PromotedLancer 
            | PromotedPawn
            deriving (Show, Eq)

-- Players
data Player = White | Black deriving (Show, Eq)

-- Taken pieces
type TakenPieces = [Piece]

-- The position of each piece is registered as a tuple of int for each location in the 9x9 board
type Position = (Int,Int)

-- Represents the movement of a piece from a position to another
type Move = (Position,Position)

-- Represents the drop of a taken piece to a position
type Drop = (Piece,Position)

-- Represents each square of the board
type Square = Maybe (Player, Piece)

-- The board is registered as a list of tuple made of the position and the piece
-- If a position is not in the list it's considered empty
type Board = Matrix Square

-- A game is recorded as a 
type Game = [Board]

-- A saved game file is made up of the taken pieces of each player and the game, if the player is in check and if the game is finished
newtype File = File (TakenPieces, TakenPieces, Game, Bool, Bool)

-- Specific string for the names of the saved games
type FileName = String
-------------------------------

data StartAction = NewG -- Starts a new game with the intial boardInit
                 | LoadG FileName -- Loads the state of the saved game with the given name
                 | PlayRec FileName -- Plays the saved game one move at the time

-- Todas las acciones que se pueden realizar mientras se esta jugando
data GameActions = MoveP Move -- Moves a piece in the board if its a legal move
                 | DropP Drop -- Puts a taken piece in a legal given position on the board
                 | PromP Position -- Promote a piece when posible
                 | ShowB -- Shows the current state of the board
                 | ShowT -- Shows the current players taken pieces
                 | SaveG FileName -- Saves the current state of the game with the given name
                 deriving Show

-- Initial board
boardInit :: Board
boardInit = fromLists [
  [Just (White, Lancer), Just (White, Knight), Just (White, Silver), Just (White, Golden), Just (White, King), Just (White, Golden), Just (White, Silver), Just (White, Knight), Just (White, Lancer)],
  [Nothing, Just (White, Rook), Nothing, Nothing, Nothing, Nothing, Nothing, Just (White, Bishop), Nothing],
  [Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Just (White, Pawn)],
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
  [Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn)],
  [Nothing, Just (Black, Bishop), Nothing, Nothing, Nothing, Nothing, Nothing, Just (Black, Rook), Nothing],
  [Just (Black, Lancer), Just (Black, Knight), Just (Black, Silver), Just (Black, Golden), Just (Black, King), Just (Black, Golden), Just (Black, Silver), Just (Black, Knight), Just (Black, Lancer)]
  ]

allPiecesTestBoard :: Board
allPiecesTestBoard = fromLists [
  [Just (White, Lancer), Just (White, Knight), Just (White, Silver), Just (White, Golden), Just (White, King), Just (White, Golden), Just (White, Silver), Just (White, Knight), Just (White, Lancer)],
  [Nothing, Just (White, Rook), Nothing, Nothing, Nothing, Nothing, Nothing, Just (White, Bishop), Nothing],
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
  [Nothing, Just (Black, Bishop), Nothing, Nothing, Nothing, Nothing, Nothing, Just (Black, Rook), Nothing],
  [Just (Black, Lancer), Just (Black, Knight), Just (Black, Silver), Just (Black, Golden), Just (Black, King), Just (Black, Golden), Just (Black, Silver), Just (Black, Knight), Just (Black, Lancer)]
  ] 
    

promotionTestBoard :: Board
promotionTestBoard = fromLists [
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
  [Nothing, Just (Black, Bishop), Nothing, Just (Black, Pawn), Just (Black, Pawn), Just (Black, Pawn), Nothing, Just (Black, Rook), Nothing],
  [Just (Black, Lancer), Just (Black, Knight), Just (Black, Silver), Just (Black, Golden), Just (Black, King), Just (Black, Golden), Just (Black, Silver), Just (Black, Knight), Just (Black, Lancer)],
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
  [Just (White, Lancer), Just (White, Knight), Just (White, Silver), Just (White, Golden), Just (White, King), Just (White, Golden), Just (White, Silver), Just (White, Knight), Just (White, Lancer)],
  [Nothing, Just (White, Rook), Nothing, Just (White, Pawn), Just (White, Pawn), Just (White, Pawn), Nothing, Just (White, Bishop), Nothing],
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  ]


checkmateDropTestBoard :: Board -- Comando a usar m (6,7) (5,8) o d s (5,8) mientras se tenga al menos un silver como pieza tomada
checkmateDropTestBoard = fromLists [
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
  [Nothing, Nothing, Just (White, Pawn), Nothing, Nothing, Nothing, Nothing, Just (Black, Silver), Nothing],
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just (White, Pawn), Nothing],
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just (White, Pawn), Just (White, King), Just (White, Pawn)],
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just (Black, Silver), Just (Black, Silver), Nothing],
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just (Black, King), Nothing]
  ]