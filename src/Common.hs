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
            deriving Show

-- Players
data Player = White | Black deriving Show

-- Taken pieces
type TakenPieces = [Piece]

-- The position of each piece is registered as a tuple of int for each location in the 9x9 board
type Position = (Integer,Integer)

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

-- A saved game file is made up of the taken pieces of each player and the game
newtype File = File (TakenPieces, TakenPieces, Game)

-- Specific string for the names of the saved games
type FileName = String
-------------------------------

-- Todas las acciones que se pueden realizar mientras se esta jugando
data GameActions = MoveP Position Position -- Moves a piece in the board if its a legal move
                 | PutP Piece Position -- Puts a taken piece in a legal given position on the board
                 | PromP Position -- Promote a piece when posible
                 | ShowB -- Shows de current state of the board
                 | SaveG FileName -- Saves the current state of the game with the given name
                 | NewG -- Starts a new game with the intial boardInit
                 | LoadG FileName -- Loads the state of the saved game with the given name
                 | PlayRec FileName -- Plays the saved game one move at the time
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
    
    
    
    
    
    
    -- Board [(White,Pos(9,1),Lancer),(White,Pos(8,1),Knight),(White,Pos(7,1),SilverG),(White,Pos(6,1),GoldenG)
    --               , (White,Pos(5,1),Goldeng),(White,Pos(4,1),GoldenG),(White,Pos(3,1),SilverG),(White,Pos(2,1),Knight),(White,Pos(1,1),Lancer)
    --               , (White,Pos(8,2),Rook),(White,Pos(2,2),Bishop),(White,Pos(9,3),Pawn),(White,Pos(8,3),Pawn),(White,Pos(7,3),Pawn)
    --               , (White,Pos(6,3),Pawn),(White,Pos(5,3),Pawn),(White,Pos(4,3),Pawn),(White,Pos(3,3),Pawn),(White,Pos(2,3),Pawn),(White,Pos(1,3),Pawn)
    --               , (Black,Pos(9,7),Pawn),(Black,Pos(8,7),Pawn),(Black,Pos(7,7),Pawn),(Black,Pos(6,7),Pawn),(Black,Pos(5,7),Pawn),(Black,Pos(4,7),Pawn)
    --               , (Black,Pos(3,7),Pawn),(Black,Pos(2,7),Pawn),(Black,Pos(1,7),Pawn),(Black,Pos(8,8),Bishop),(Black,Pos(2,8),Rook)
    --               , (Black,Pos(9,9),Lancer),(Black,Pos(8,9),Knight),(Black,Pos(7,9),SilverG),(Black,Pos(6,9),GoldenG),(Black,Pos(5,9),Goldeng)
    --               , (Black,Pos(4,9),GoldenG),(Black,Pos(3,9),SilverG),(Black,Pos(2,9),Knight),(Black,Pos(1,9),Lancer)]
