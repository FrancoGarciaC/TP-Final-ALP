{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use hPrint" #-}


module MonadShogi(
    Shogi,
    showBoard,
    printPlay,
    getLineShogi,
    appendShogi,
    newGame,
    loadGame,
    replayGame,
    replayBoard,
    printShogi,
    printShogiLn,
    printPlayerInCheck,
    printPlayerCheckmate,
    showTakenPieces,
    getCurrentBoard,
    getWinStatus,
    getPieceInPromotionZone,
    getCurrentPlayer,
    getCurrentPlayerInCheck,
    getBlackTakenPieces,
    getWhiteTakenPieces,
    getTakenPieces,
    changeCurrentTakenPieces,
    addTakenPieceBlack,
    addTakenPieceWhite,
    addTakenPiece,
    removeTakenPieceBlack,
    removeTakenPieceWhite,
    removeTakenPiece,
    changeCheckStatus,
    changeCurrentBoard,
    changeCurrentPlayer,
    changeWinStatus,
    changeMoveCanPromote,
    addMove,
    getGame,
    changeGame,
    takeMove,
    saveBoard,
    exportShogi,
    resetShogi,
    failShogi,
    runShogi,
    catchErrors,
    MonadShogi,
    module Control.Monad.Except,
    module Control.Monad.State)
 where

import Data.Matrix
import Common
import Global
import Parser ( deparseBoard )
import Errors( Error (..) )
import Data.List (delete)
import Control.Monad.State
import Control.Monad.Except
import System.IO
import Data.Maybe
import Prelude
import System.Directory ( copyFile, doesFileExist, )


class (MonadIO m, MonadState GameState m, MonadError Error m) => MonadShogi m where
appendShogi :: MonadShogi m => FilePath -> String -> m()
appendShogi p s = liftIO $ appendFile p s

printShogi :: MonadShogi m => String -> m ()
printShogi = liftIO . putStr

printShogiLn :: MonadShogi m => String -> m ()
printShogiLn = liftIO . putStrLn

getLineShogi :: MonadShogi m => m String
getLineShogi = liftIO $ getLine

showBoard :: MonadShogi m => m ()
showBoard = do b <- getCurrentBoard
               showBoard' b (1,1)

showBoard' :: MonadShogi m => Board -> Position -> m ()
showBoard' b (9,9) = do case b!(9,9) of
                         Nothing -> do printShogiLn "  "
                         Just (c,p) -> do printShogiLn (showPiece c p)
showBoard' b (x,9) = do case b!(x,9) of
                         Nothing -> do printShogiLn "  "
                                       showBoard' b (x+1,1)
                         Just (c,p) -> do printShogiLn (showPiece c p)
                                          showBoard' b (x+1,1)
showBoard' b (x,y) = do case b!(x,y) of
                         Nothing -> do printShogi "  "
                                       showBoard' b (x,y+1)
                         Just (c,p) -> do printShogi (showPiece c p)
                                          showBoard' b (x,y+1)

showPiece :: Player -> Piece -> String
showPiece p Pawn = showPlayer p ++ "P"
showPiece p PromotedPawn = showPlayer p ++ "PP"
showPiece p Lancer = showPlayer p ++ "L"
showPiece p PromotedLancer = showPlayer p ++ "PL"
showPiece p Knight = showPlayer p ++ "Kn"
showPiece p PromotedKnight = showPlayer p ++ "PK"
showPiece p Rook = showPlayer p ++ "R"
showPiece p Dragon = showPlayer p ++ "D"
showPiece p Bishop = showPlayer p ++ "B"
showPiece p Horse = showPlayer p ++ "H"
showPiece p Silver = showPlayer p ++ "S"
showPiece p PromotedSilver = showPlayer p ++ "PS"
showPiece p Golden = showPlayer p ++ "G"
showPiece p King = showPlayer p ++ "K"



showPlayer :: Player -> String
showPlayer Black = "B"
showPlayer White = "W"

showTakenPieces :: MonadShogi m => Player -> m ()
showTakenPieces c = do p <- getTakenPieces c
                       printShogiLn (show p)

printPlay :: MonadShogi m => m ()
printPlay = do p <- getCurrentPlayer
               case p of
                Black -> printShogiLn "Jugador 1 cual es su jugada?"
                White -> printShogiLn "Jugador 2 cual es su jugada?"

printPlayerInCheck :: MonadShogi m => Player -> m ()
printPlayerInCheck Black = printShogiLn "Jugador 2 está en jaque"
printPlayerInCheck White = printShogiLn "Jugador 1 está en jaque"

printPlayerCheckmate :: MonadShogi m =>  m ()
printPlayerCheckmate = do c <- getCurrentPlayer
                          case c of
                            Black -> printShogiLn "Felicitaciones jugador 1 ganaste la partida!"
                            White -> printShogiLn "Felicitaciones jugador 2 ganaste la partida!"

newGame :: MonadShogi m => m ()
newGame = modify (\s -> s {playerInCheck = False, wonGame = False, moveCanPromote = False, currentBoard = boardInit, currentGame = [boardInit], currentTakenPieces = ([],[]), currentPlayer = Black})

loadGame :: MonadShogi m => Game -> (TakenPieces,TakenPieces) -> Player -> Bool -> Bool -> m ()
loadGame xs tp player check won = let xs' = reverse xs
                                   in do saveGame xs
                                         modify (\s -> s {playerInCheck = check, wonGame = won, moveCanPromote = False, currentBoard = head xs', currentGame = tail xs', currentTakenPieces = tp, currentPlayer = player})

replayGame :: MonadShogi m => Game -> m ()
replayGame xs = modify (\s -> s {playerInCheck = False, wonGame = False, moveCanPromote = False, currentBoard = head xs, currentGame = tail xs})

replayBoard :: MonadShogi m => m ()
replayBoard = do takeMove -- TO DO Analizar los casos y si da nothing tiene que pasar a pantalla de victoria
                 showBoard

getCurrentBoard :: MonadShogi m => m Board
getCurrentBoard = gets currentBoard

getCurrentPlayer :: MonadShogi m => m Player
getCurrentPlayer = gets currentPlayer

getCurrentPlayerInCheck :: MonadShogi m => m Bool
getCurrentPlayerInCheck = gets playerInCheck

getWinStatus :: MonadShogi m => m Bool
getWinStatus = gets wonGame

getPieceInPromotionZone :: MonadShogi m => m Bool
getPieceInPromotionZone = gets moveCanPromote

getBlackTakenPieces :: MonadShogi m => m TakenPieces
getBlackTakenPieces = do tp <- gets currentTakenPieces
                         return (fst tp)

getWhiteTakenPieces :: MonadShogi m => m TakenPieces
getWhiteTakenPieces = do tp <- gets currentTakenPieces
                         return (snd tp)

getTakenPieces :: MonadShogi m => Player -> m TakenPieces
getTakenPieces Black = getBlackTakenPieces
getTakenPieces White = getWhiteTakenPieces

changeCurrentTakenPieces :: MonadShogi m => TakenPieces -> Player -> m ()
changeCurrentTakenPieces tp Black = do tpW <- getWhiteTakenPieces
                                       modify (\s -> s {currentTakenPieces = (tp,tpW)})
changeCurrentTakenPieces tp White = do tpB <- getBlackTakenPieces
                                       modify (\s -> s {currentTakenPieces = (tpB,tp)})

-- Agrega una pieza a las tomadas de Black
addTakenPieceBlack :: MonadShogi m => Piece -> m ()
addTakenPieceBlack p = do tp <- getBlackTakenPieces
                          changeCurrentTakenPieces ([p]++tp) Black

-- Agrega una pieza a las tomadas de White
addTakenPieceWhite :: MonadShogi m => Piece -> m ()
addTakenPieceWhite p = do tp <- getWhiteTakenPieces
                          changeCurrentTakenPieces ([p]++tp) White

addTakenPiece :: MonadShogi m => Piece -> Player -> m ()
addTakenPiece p Black = addTakenPieceBlack p
addTakenPiece p White = addTakenPieceWhite p

removeTakenPieceBlack :: MonadShogi m => Piece -> m ()
removeTakenPieceBlack p = do tp <- getBlackTakenPieces
                             changeCurrentTakenPieces (delete p tp) Black

removeTakenPieceWhite :: MonadShogi m => Piece -> m ()
removeTakenPieceWhite p = do tp <- getWhiteTakenPieces
                             changeCurrentTakenPieces (delete p tp) White

removeTakenPiece :: MonadShogi m => Piece -> Player -> m ()
removeTakenPiece p Black = removeTakenPieceBlack p
removeTakenPiece p White = removeTakenPieceWhite p

changeCheckStatus :: MonadShogi m => Bool -> m ()
changeCheckStatus b = modify (\s -> s {playerInCheck = b})

changeCurrentBoard :: MonadShogi m => Board -> m ()
changeCurrentBoard b = modify (\s -> s {currentBoard = b}) 

changeCurrentPlayer :: MonadShogi m => m ()
changeCurrentPlayer = do player <- getCurrentPlayer
                         case player of
                              White -> modify  (\s -> s {currentPlayer = Black})
                              Black -> modify  (\s -> s {currentPlayer = White})

changeWinStatus :: MonadShogi m => Bool -> m ()
changeWinStatus b = do modify (\s -> s {wonGame = b})

changeMoveCanPromote :: MonadShogi m => Bool -> m ()
changeMoveCanPromote b = modify (\s -> s {moveCanPromote = b})
-- CurrentBoard muestra siempre el ultimo movimiento valido entonces debemos cambiarlo
-- y luego agregar este nuevo movimiento a currentGame, la lista con todos los movimientos realizados
addMove :: MonadShogi m => Board -> m ()
addMove b = do changeCurrentBoard b
               modify (\s -> case currentGame s of 
                                  xs -> s {currentGame = b : xs})
               
-- Tener en cuenta que la lista esta ordenada de ultimo movimiento al primero
getGame :: MonadShogi m => m Game
getGame = gets currentGame

changeGame :: MonadShogi m => Game -> m ()
changeGame g = modify (\s -> s {currentGame = g})

-- Quita el ultimo movimiento de la lista de Game y la retorna
takeMove :: MonadShogi m => m (Maybe Board)
takeMove = do g <- getGame
              case g of
                [] -> return Nothing
                (x:xs) -> do changeGame xs
                             changeCurrentBoard x
                             return (Just x)

-- Escribe el tablero actual currentGame.txt                     
saveBoard :: MonadShogi m => m ()
saveBoard = do b <- getCurrentBoard
               appendShogi "Games/currentGame.txt" (deparseBoard b ++ "\n")

saveGame :: MonadShogi m => Game -> m ()
saveGame [] = return ()
saveGame (x:xs) = do appendShogi "Games/currentGame.txt" (deparseBoard x ++ "\n")
                     saveGame xs


-- Exporta una partida a un archivo con un nombre dado
exportShogi :: MonadShogi m => String -> m ()
exportShogi fn = do b <- liftIO $ doesFileExist "Games/currentGame.txt"
                    if b
                    then liftIO $ copyFile "Games/currentGame.txt" ("Games/" ++ fn ++ ".txt")
                    else return ()

resetShogi :: MonadShogi m => m ()
resetShogi = do b <- liftIO $ doesFileExist "Games/currentGame.txt"
                if b
                then liftIO $ writeFile "Games/currentGame.txt" ""
                else return ()


failShogi :: MonadShogi m => String -> m a
failShogi message = throwError (ActionError message ) 

type Shogi = StateT GameState (ExceptT Error IO)

instance MonadShogi Shogi

runShogi' :: Shogi a -> IO(Either Error (a,GameState))
runShogi' c = runExceptT $ runStateT c initialState

runShogi :: Shogi a -> IO(Either Error a)
runShogi c = fmap fst <$> runShogi' c

catchErrors :: MonadShogi m => m a -> m (Maybe a)
catchErrors c = catchError (Just <$> c)
                           (\e -> liftIO $ hPutStrLn stderr (show e) >> return Nothing)