{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use hPrint" #-}


module MonadShogi(
    Shogi,
    newGame,
    loadGame,
    replayGame,
    getCurrentBoard,
    getCurrentPlayer,
    changeCurrentBoard,
    changeCurrentPlayer,
    addMove,
    getGame,
    failShogi,
    runShogi,
    catchErrors,
    MonadShogi,
    module Control.Monad.Except,
    module Control.Monad.State)
 where

import Common
import Global
import Errors( Error (..) )
import Control.Monad.State
import Control.Monad.Except
import System.IO
import Data.Maybe
import Control.Monad.RWS (modify)
import Prelude
import Distribution.TestSuite (Result(Error))


class (MonadIO m, MonadState GameState m, MonadError Error m) => MonadShogi m where
printShogi :: MonadShogi m => String -> m ()
printShogi = liftIO . putStrLn

newGame :: MonadShogi m => m ()
newGame = modify (\s -> s {gameInSession = True, replayInSession = False, currentBoard = boardInit, currentGame = [boardInit], currentTakenPieces = ([],[]), currentPlayer = Black})

loadGame :: MonadShogi m => Game -> (TakenPieces,TakenPieces) -> Player -> m ()
loadGame xs tp player = modify (\s -> s {gameInSession = True, replayInSession = False, currentBoard = head xs, currentGame = xs, currentTakenPieces = tp, currentPlayer = player})

replayGame :: MonadShogi m => Game -> m ()
replayGame xs = let moves = reverse xs in
                           modify (\s -> s {gameInSession = False, replayInSession = True, currentBoard = head moves, currentGame = moves})

getCurrentBoard :: MonadShogi m => m Board
getCurrentBoard = gets currentBoard

getCurrentPlayer :: MonadShogi m => m Player
getCurrentPlayer = gets currentPlayer

changeCurrentBoard :: MonadShogi m => Board -> m ()
changeCurrentBoard b = modify (\s -> s {currentBoard = b}) 

changeCurrentPlayer :: MonadShogi m => m ()
changeCurrentPlayer = do player <- getCurrentPlayer
                         case player of
                              White -> modify  (\s -> s {currentPlayer = Black})
                              Black -> modify  (\s -> s {currentPlayer = White})


-- CurrentBoard muestra siempre el ultimo movimiento valido entonces debemos cambiarlo
-- y luego agregar este nuevo movimiento a currentGame, la lista con todos los movimientos realizados
addMove :: MonadShogi m => Board -> m ()
addMove b = do changeCurrentBoard b
               modify (\s -> case currentGame s of 
                                  xs -> s {currentGame = b : xs})
               
-- Tener en cuenta que la lista esta ordenada de ultimo movimiento al primero
getGame :: MonadShogi m => m Game
getGame = gets currentGame

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