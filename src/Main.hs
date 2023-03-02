{-# LANGUAGE FlexibleContexts #-}
module Main where

import Common
import Parser
import Eval
import System.Exit
import System.Console.Haskeline
import Data.Char (toLower)
import Data.ByteString (putStrLn, getLine)
import MonadShogi
-- Hacer un menu con las opciones principales de new juego, cargar juego, reproducir juego, se ingresa cual opcion prefiere y pasa a un modo interactivo con el tablero acorde 
main :: IO ()
main = do Prelude.putStrLn ("Bienvenido al Simulador de Shogi. \n"
                        ++ "Desea jugar un partida nueva? O jugar o reproducir un partida guardada? \n"
                        ++ "Escriba help si necesita ayuda.")
          minput <- Prelude.getLine
          case map toLower minput of
              "" -> main
              "exit" -> return ()
              "help" -> Prelude.putStrLn ("Los comandos para comenzar son: \n"
                                   ++ "NewGame para una partida nueva \n"
                                   ++ "LoadGame seguido del nombre del archivo para cargar una partida guardada \n"
                                   ++ "PlayRecord seguido del nombre del archivo para reproducir una partida guardada.")
              input -> do case mainActionParser input of
                               Left s -> do Prelude.putStrLn "Error de parseo verifique que haya escrito correctamente su comando inicial \n"
                                            main
-- Analizar todos los casos de action, para newG solo generar un shogi, 
-- para los otros dos se tienen que leer los archivos y con eso hacer un shogi
-- para pasarle a gameInSession para que trabaje directamente con estado correspondiente
                               Right action -> do runShogi(runInputT defaultSettings (gameInSession action))
                                                  return ()

gameInSession :: (MonadShogi m) => GameActions -> InputT m ()
gameInSession action = do eval action
                          getCurrentBoard
                          getCurrentPlayer
                          gameMenu
    where
        playingGame = do minput <- getInputLine "% "
                         input <- map toLower minput
                         case gameActionParser input of
                             Left m -> do print m
                                          playingGame
                             Right a -> do eval a
                                           ShowBoard
                                           playingGame 