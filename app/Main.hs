{-# LANGUAGE FlexibleContexts #-}
module Main where

import Common
import Parser
import Eval
import System.Exit
import System.Console.Haskeline 
import Data.Char (toLower)
import System.Directory ( removeFile, doesFileExist )
import MonadShogi
import Control.Monad.Catch (MonadMask)
-- Hacer un menu con las opciones principales de new juego, cargar juego, reproducir juego, se ingresa cual opcion prefiere y pasa a un modo interactivo con el tablero acorde 
main :: IO ()
main = do putStrLn ("Bienvenido al Simulador de Shogi. \n"
                        ++ "Desea jugar un partida nueva? O jugar o reproducir un partida guardada? \n"
                        ++ "Escriba help si necesita ayuda.")
          runShogi(runInputT defaultSettings startSession)
          return ()
                               

startSession :: (MonadShogi m, MonadMask m) => InputT m ()
startSession = do minput <- getInputLine "% "
                  case minput of
                      Nothing -> return ()
                      Just "" -> startSession
                      Just "exit" -> return ()
                      Just "help" -> do outputStrLn ("Los comandos para comenzar son: \n"
                                           ++ "n o New para una partida nueva \n"
                                           ++ "l o Load seguido del nombre del archivo para cargar una partida guardada \n"
                                           ++ "r o Record seguido del nombre del archivo para reproducir una partida guardada.")
                                        startSession
                      Just input -> case mainActionParser input of
                                        Left _ -> do outputStrLn "Error de parseo verifique que haya escrito correctamente su comando inicial" 
                                                     startSession
                                        Right a@(PlayRec _) -> do e <- lift $ catchErrors (evalStart a)
                                                                  case e of
                                                                    Nothing -> startSession
                                                                    Just _ -> do exist <- liftIO $ doesFileExist "Games/currentGame.txt"
                                                                                 when exist $ liftIO $ removeFile "Games/currentGame.txt"
                                                                                 liftIO $ exitSuccess
                                        Right action -> do e <- lift $ catchErrors (evalStart action)
                                                           case e of
                                                             Nothing -> startSession
                                                             Just _ -> do lift $ saveBoard
                                                                          lift $ printPlay
                                                                          gameInSession
                                                                          exist <- liftIO $ doesFileExist "Games/currentGame.txt"
                                                                          when exist $ liftIO $ removeFile "Games/currentGame.txt"
                                                                          liftIO $ exitSuccess   
        where
        

gameInSession :: (MonadShogi m, MonadMask m) => InputT m ()
--gameInSession a@(PlayRec _) = lift $ eval a -- Como el evaluador se encarga de reproducir toda la partida solo se hace esto
gameInSession = do win <- lift $ getWinStatus
                   when win victoryScreen
                   minput <- getInputLine "% "
                   case minput of
                      Nothing -> return ()
                      Just "exit" -> return ()
                      Just "" -> gameInSession
                      Just s -> case gameActionParser s of
                                  Left m -> do outputStrLn $ show m
                                               gameInSession
  -- Chequear que si es un movimiento que hace que una pieza quede en zona de promocion pregunte si la desea promover
                                  Right a@(MoveP (_,f)) -> do e <- lift $ catchErrors (eval a)
                                                              case e of
                                                                  Nothing -> gameInSession
                                                                  Just _ -> do canProm <- lift $ getPieceInPromotionZone
                                                                               if (canProm)
                                                                                   then do outputStrLn "¿Desea promover la pieza que acaba de mover?"
                                                                                           promotionOption f  
                                                                                   else do gameInSession
                                  Right a@(DropP _) -> do _ <- lift $ catchErrors (eval a)
                                                          gameInSession
                                  Right a@(SaveG _) -> do e <- lift $ catchErrors (eval a)
                                                          case e of
                                                                  Nothing -> gameInSession
                                                                  Just _ -> do outputStrLn "¿Desea seguir jugando?"
                                                                               checkIfContinue
                                  Right a@(ShowB) -> do lift $ eval a
                                                        gameInSession
                                  Right a@(ShowT) -> do lift $ eval a
                                                        gameInSession
                                  Right _ -> outputStrLn "Error input inesperado"
        where
         -- Si el jugador decide promover la pieza se ejecuta directamente el comando para evaluar la promocion                      
        promotionOption pos = do minput <- getInputLine "% "
                                 case minput of
                                    Nothing -> return ()
                                    Just "" -> gameInSession
                                    Just "exit" -> return ()
                                    Just s -> let input = map toLower s
                                               in case parseConfirmation input of
                                                    Left _ -> do outputStrLn "Error de input, escriba si desea o no promover la pieza."
                                                                 promotionOption pos
                                                    Right False -> do lift $ saveBoard
                                                                      lift $ changeCurrentPlayer
                                                                      lift $ printPlay
                                                                      gameInSession
                                                    Right True -> do lift $ eval (PromP pos) -- No le pedimos al jugador escribir el comando de promocion
                                                                     gameInSession
        victoryScreen = do lift $ printPlayerCheckmate
                           outputStrLn "¿Desea guardar la partida que acaba de jugar?"
                           wishToSave
        wishToSave = do minput <- getInputLine "% "
                        case minput of
                           Nothing -> do liftIO $ removeFile "Games/currentGame.txt"
                                         liftIO $ exitSuccess
                           Just "" -> wishToSave
                           Just "exit" -> do liftIO $ removeFile "Games/currentGame.txt"
                                             liftIO $ exitSuccess
                           Just s -> let input = map toLower s
                                      in case parseConfirmation input of
                                          Left _ -> do outputStrLn "Error de input, escriba si desea o no guardar la partida"
                                                       wishToSave
                                          Right False -> do liftIO $ removeFile "Games/currentGame.txt"
                                                            liftIO $ exitSuccess
                                          Right True -> do outputStrLn "¿Con qué nombre quiere guardar la partida?"
                                                           getSaveName
        getSaveName = do minput <- getInputLine "% "
                         case minput of
                            Nothing -> do liftIO $ removeFile "Games/currentGame.txt"
                                          liftIO $ exitSuccess
                            Just "" -> getSaveName
                            Just "exit" -> do liftIO $ removeFile "Games/currentGame.txt"
                                              liftIO $ exitSuccess
                            Just name -> do e <- lift $ catchErrors (eval (SaveG name)) -- No le pedimos al jugador escribir el comando de guardar partida
                                            case e of
                                              Nothing -> do outputStrLn "Inserte otro nombre para su partida"
                                                            getSaveName
                                              Just _ -> do exist <- liftIO $ doesFileExist $ "Games/" ++ name ++ ".txt"
                                                           if exist
                                                           then do outputStrLn "Partida guardada exitosamente"
                                                                   liftIO $ removeFile "Games/currentGame.txt"
                                                                   liftIO $ exitSuccess
                                                           else do outputStrLn "La partida no pudo guardarse correctamente inserte otro nombre para su partida"
                                                                   getSaveName
        checkIfContinue = do minput <- getInputLine "% "
                             case minput of
                                Nothing -> do liftIO $ removeFile "Games/currentGame.txt"
                                              liftIO $ exitSuccess
                                Just "" -> checkIfContinue
                                Just s -> case parseConfirmation s of
                                            Left _ -> do outputStrLn "Error de input, escriba si desea o no seguir jugando"
                                                         checkIfContinue
                                            Right False -> do liftIO $ removeFile "Games/currentGame.txt"
                                                              liftIO $ exitSuccess
                                            Right True -> do lift $ printPlay
                                                             gameInSession
        -- playReplay = do waitForAnyKey ""
        --                 m <- lift $ takeMove
        --                 case m of
        --                   Nothing -> do outputStrLn "Termino la reproduccion de la partida"
        --                                 liftIO $ exitSuccess
        --                   Just _ -> do lift $ showBoard
        --                                playReplay
