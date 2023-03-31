module Eval(
  eval,
  evalStart
) where
import Common
import MonadShogi
import Parser
import Data.Matrix
-- import Distribution.TestSuite (Result(Error))
-- import Errors
-- import Text.Parsec (runParser)
-- import System.IO.Error (isDoesNotExistError)
import System.Directory ( doesFileExist )
import Control.Exception (catch, IOException)
import System.IO

evalStart :: MonadShogi m => StartAction -> m ()
evalStart NewG = do resetShogi
                    newGame
-- Usar catch errors con un handler (aun no definido) en el main y usar throws para indicar lo errores del eval
evalStart (LoadG name) = do resetShogi
                            loadedGame <- liftIO $ catch (readFile ("Games/" ++ name ++ ".txt"))
                                                 (\e -> do let err = (e :: IOException) 
                                                           hPutStrLn stderr ("La partida " ++ name ++ " que se quiere cargar no existe")
                                                           return "")
                            let file = fileParser loadedGame
                             in case file of
                               Left e -> failShogi (show e)--("Error en el parseo de la partida " ++ name)
                               Right (File (tpBlack, tpWhite, boards, check, win)) -> 
                                 let player = if even (length boards) then White else Black
                                  in do loadGame boards (tpBlack,tpWhite) player check win
                                        case win of
                                          True -> failShogi "Esa partida ya esta finalizada"
                                          False -> do showBoard
evalStart (PlayRec name) = do loadedReplay <- liftIO $ catch (readFile ("Games/" ++ name ++ ".txt"))
                                                     (\e -> do let err = (e :: IOException) 
                                                               hPutStrLn stderr ("El replay " ++ name ++ " que se quiere cargar no existe")
                                                               return "")
                              let file = fileParser loadedReplay
                               in case file of
                               Left _ -> failShogi ("Error en el parseo del replay " ++ name)
                               Right (File (_, _,boards,_,_)) -> do replayGame boards
                                                                    showBoard
                                                                    printShogiLn "Presione enter para pasar al siguente turno o exit para salir del juego"
                                                                    replayRecord
-- Redefinir el comportamiento para playrec para que quede en un loop esperando un
-- click o boton para que vaya pasando cada movimiento sin depender de otro input
eval :: MonadShogi m => GameActions -> m ()
eval (MoveP p@(pI,pF)) = do b <- getCurrentBoard
                            colour <- getCurrentPlayer
                            if (not(isValidMove b pI pF) || not(canUsePiece b pI colour))
                              then do failShogi $ "Movimiento invalido para la pieza : " ++ show (b!pI)
                              else do isInCheck <- getCurrentPlayerInCheck
                                      if not (isInCheck)
                                        then makeMove (pI,pF)
                                        else let kingPos = findKing b colour (1,1)
                                        -- Probamos si el movimiento hace que salga del jaque
                                              in case tryBoardsCheck b kingPos colour [(pI,pF)] [] of
                                                -- Caso en que el movimiento no haga que salga del jaque y tenga que hacer otro
                                                  True -> failShogi "Esta en jaque, haga otro movimiento o puede poner una pieza" 
                                                  False -> makeMove (pI,pF) -- El movimiento lo saque del jaque

-- Como la promocion se hace luego de mover una pieza no se tiene que chequear si el jugador esta en jaque como se hace con MoveP
eval (PromP pos) = do b <- getCurrentBoard 
                      if (canPromote b pos)
                        then makePromotion pos
                        else failShogi "No puede promover esa pieza" -- No deberia de imprimir esto nunca

eval (DropP (piece,pos)) = do b <- getCurrentBoard
                              c <- getCurrentPlayer
                              canD <- canDrop b pos c piece
                              if not (canD)
                               then failShogi "No puede poner esa pieza alli"
                               else do isInCheck <- getCurrentPlayerInCheck
                                       if not (isInCheck)
                                        then makeDrop (piece,pos)
                                        else let kingPos = findKing b c (1,1)
                                        -- Probamos si el drop hace que salga del jaque
                                            in case tryBoardsCheck b kingPos c [] [(piece,pos)] of
                                                  True -> failShogi "Esta en jaque, debe poner otra pieza o hacer un movimiento"
                                                  False -> makeDrop (piece,pos)
-- Guarda en el archivo de partida actual el estado de la partida
-- TO DO cambiar que primero copie todo el archivo de current game a fn y le agregue con append shogi las tp y check         
eval (SaveG fn) = do e <- liftIO $ doesFileExist ("Games/" ++ fn ++ ".txt")
                     if e then failShogi "Ya existe una partida con ese nombre"
                     else do tpB <- getBlackTakenPieces
                             tpW <- getWhiteTakenPieces
                             check <- getCurrentPlayerInCheck
                             win <- getWinStatus
                             exportShogi fn -- Creamos una copia del juego actual y le agregamos los datos necesarios para luego cargar la partida
                             appendShogi ("Games/" ++ fn ++ ".txt") $ "\n-\n"
                             appendShogi ("Games/" ++ fn ++ ".txt") $ deparseTakenPieces tpB ++ " "
                             appendShogi ("Games/" ++ fn ++ ".txt") $ deparseTakenPieces tpW ++ " "
                             appendShogi ("Games/" ++ fn ++ ".txt") $ show check ++ " "
                             appendShogi ("Games/" ++ fn ++ ".txt") $ show win ++ " "
eval ShowB = showBoard           
eval ShowT = do c <- getCurrentPlayer
                showTakenPieces c

-- Genera el tablero resultante de hacer un movimiento y actualiza el estado segun sea necesario
makeMove :: MonadShogi m => Move -> m ()
makeMove m@(_,pF) = do b <- getCurrentBoard -- Tomamos los elementos del estado actual
                       colour <- getCurrentPlayer
                       let b' = tryBoardMove b m -- Generamos el tablero resultante
                           stillChecked = isKingChecked b' pF -- Prevenimos de hacer movimientos que no saquen al rey del jaque
                           check = isChecking b' colour pF -- Vemos si el ultimo movimiento puso en jaque al rey enemigo
                           canP = canPromote b' pF -- Vemos si el movimiento puso una pieza promovible en la zona de promocion
                        in do when stillChecked $ failShogi "El rey sigue en jaque con ese movimiento"
                              changeMoveCanPromote (canP) -- Actualizamos el estado si entro a la zona de promocion
                              changeCheckStatus (check) -- Cambiamos el estado de check sino se mantiene el mismo
                              when check $ checkCheckmate b' colour -- Si se esta en check se verifica si es checkmate y cambia el estado acorde
                              case (b!pF) of
                                -- Caso en que solo se mueve la pieza
                                Nothing -> do addMove b' -- Cambiamos el tablero a pesar de si se puede hacer promocion o no
                                              if canP then return ()
                                              else do showBoard
                                                      saveBoard
                                                      didWin <- getWinStatus
                                                      when (not canP && not didWin) changeCurrentPlayer -- Si no entro se puede cambiar el jugador para pasar al siguiente turno
                                                      when (not didWin) $ printPlay
                                -- Caso en que el movimiento hace que se capture una pieza enemiga
                                Just (_,p) -> do addTakenPiece p colour
                                                 addMove b' -- Cambiamos el tablero a pesar de si se puede hacer promocion o no
                                                 if canP then return ()
                                                 else do showBoard
                                                         saveBoard
                                                         didWin <- getWinStatus
                                                         when (not canP && not didWin) changeCurrentPlayer -- Si no entro se puede cambiar el jugador para pasar al siguiente turno
                                                         when (not didWin) $ printPlay
 
isKingChecked :: Board -> Position -> Bool
isKingChecked b pos = case b!pos of
                        Nothing -> False
                        Just (_,King) -> isCheck' b pos (1,1)
                        Just _ -> False
 
checkCheckmate :: MonadShogi m => Board -> Player -> m ()
checkCheckmate b Black = do cm <- isCheckMate b White
                            case cm of
                              False -> printPlayerInCheck Black
                              True -> do changeWinStatus True
                                         return ()
checkCheckmate b White = do cm <- isCheckMate b Black
                            case cm of
                              False -> printPlayerInCheck White
                              True -> do changeWinStatus True
                                         return ()


-- Genera el tablero resultante de hacer un drop y actualiza el estado segun sea necesario
makeDrop :: MonadShogi m => Drop -> m ()
makeDrop (piece,pos) = do b <- getCurrentBoard
                          c <- getCurrentPlayer
                          let b' = setElem (Just (c,piece)) pos b
                              check = isChecking b' c pos
                              stillChecked = isCheck b' c
                            in do when stillChecked $ failShogi "El rey sigue en jaque con ese movimiento"
                                  if not check
                                    then do addMove b'
                                            showBoard
                                            saveBoard
                                            didWin <- getWinStatus
                                            when (not didWin) $ changeCurrentPlayer
                                            when (not didWin) $ printPlay
                                    else do checkCheckmate b' c
                                            addMove b'
                                            showBoard
                                            saveBoard
                                            didWin <- getWinStatus
                                            when (not didWin) $ changeCurrentPlayer
                                            when (not didWin) $ printPlay

-- Genera el tablero resultante de hacer una promocion y actualiza el estado segun sea necesario
makePromotion :: MonadShogi m => Position -> m ()
makePromotion pos = do c <- getCurrentPlayer
                       changeCurrentPlayer -- Cambiamos el jugador para el proximo turno
                       changeMoveCanPromote False
                       b' <- promotePiece pos
                       if not(isChecking b' c pos) -- Verificamos si la promocion puso en jaque al rey
                        then do addMove b'
                                showBoard
                                saveBoard
                                printPlay
                        else do checkCheckmate b' c
                                addMove b'
                                showBoard
                                saveBoard
                                printPlay

promotePiece :: MonadShogi m => Position -> m Board
promotePiece pos = do b <- getCurrentBoard
                      case b!pos of
                       Just (c,Pawn) -> return (setElem (Just (c,PromotedPawn)) pos b)
                       Just (c,Lancer) -> return (setElem (Just (c,PromotedLancer)) pos b)
                       Just (c,Knight) -> return (setElem (Just (c,PromotedKnight)) pos b)
                       Just (c,Silver) -> return (setElem (Just (c,PromotedSilver)) pos b)
                       Just (c,Rook) -> return (setElem (Just (c,Dragon)) pos b)
                       Just (c,Bishop) -> return (setElem (Just (c,Horse)) pos b)
                       _ -> return b

-- Reproduce una partida guardada y espera a que presionen enter para pasar de turno
replayRecord :: MonadShogi m => m ()
replayRecord = do s <- getLineShogi
                  case s of
                    "exit" -> return ()
                    _ -> do m <- takeMove
                            case m of 
                              Nothing -> printShogiLn "Termino la reproduccion de la partida"
                              Just _ -> do showBoard
                                           replayRecord

-- Verifica si se puede realizar el movimiento solicitado
isValidMove :: Board -> Position -> Position -> Bool
isValidMove board pI@(xI,yI) pF@(xF,yF) = if ((xI > 9) || (yI > 9) || (xF > 9) || (yF > 9) || (xI < 1) || (yI < 1) || (xF < 1) || (yF < 1) || ((xI == xF) && (yI == yF))) then False
                                    else case board!pI of
                                            Nothing -> False
                                            Just (Black,Pawn) -> if ((xI-1 /= xF) || (yI /= yF)) then False
                                                                 else isNotSameColor (board!pF) Black
                                            Just (White,Pawn) -> if ((xI+1 /= xF) || (yI /= yF)) then False
                                                                 else isNotSameColor (board!pF) White
                                            Just (Black,Lancer) -> if (xI <= xF || yI /= yF) then False
                                                                   else isNotSameColor (board!pF) Black
                                            Just (White,Lancer) -> if (xI >= xF || yI /= yF) then False
                                                                   else isNotSameColor (board!pF) White
                                            Just (Black,Knight) -> if (((yI+1 == yF) || (yI-1 == yF)) && (xI-2 == xF)) then isNotSameColor (board!pF) Black
                                                                   else False
                                            Just (White,Knight) -> if (((yI+1 == yF) || (yI-1 == yF)) && (xI+2 == xF)) then isNotSameColor (board!pF) White
                                                                   else False
                                            Just (Black,Silver) -> if ((xI-1 == xF && ((yI-1 == yF) || (yI == yF) || (yI+1 == yF))) || (xI+1 == xF && ((yI-1 == yF) || (yI+1 == yF))))
                                                                   then isNotSameColor (board!pF) Black
                                                                   else False
                                            Just (White,Silver) -> if ((xI-1 == xF && ((yI-1 == yF) || (yI+1 == yF))) || (xI+1 == xF && ((yI-1 == yF) || (yI == yF) || (yI+1 == yF))))
                                                                   then isNotSameColor (board!pF) White
                                                                   else False
                                            Just (c,Bishop) -> if ((xI == xF) || (yI == yF)) then False
                                                                   else isNotSameColor (board!pF) c
                                            Just (c,Rook) -> if ((xI /= xF) && (yI /= yF)) then False
                                                             else isNotSameColor (board!pF) c
                                            Just (c,King) -> if ((xI+1 == xF) || (xI == xF) || (xI-1 == xF)) && ((yI+1 == yF) || (yI == yF) || (yI-1 == yF))
                                                             then isNotSameColor (board!pF) c
                                                             else False
                                            Just (c,Dragon) -> if ((xI == xF) || (yI == yF) || (xI-1 == xF && ((yI-1 == yF) || (yI+1 == yF))) || (xI+1 == xF && ((yI-1 == yF) || (yI+1 == yF))))
                                                               then isNotSameColor (board!pF) c
                                                               else False
                                            Just (c,Horse) -> if ((xI /= xF) || (yI /= yF) || (((xI+1 == xF) || (xI-1 == xF)) && yI == yF) || (xI == xF && ((yI-1 == yF) || (yI+1 == yF))))
                                                              then isNotSameColor (board!pF) c
                                                              else False
                                            Just (Black,_) -> if ((xI-1 == xF && ((yI-1 == yF) || (yI == yF) || (yI+1 == yF))) || (xI == xF && ((yI-1 == yF) || (yI+1 == yF))) || ((xI+1 == xF) && (yI == yF)))
                                                              then isNotSameColor (board!pF) Black
                                                              else False
                                            Just (White,_) -> if (((xI-1 == xF) && (yI == yF)) || (xI == xF && ((yI-1 == yF) || (yI+1 == yF))) || (xI+1 == xF && ((yI-1 == yF) || (yI == yF) || (yI+1 == yF))))
                                                              then isNotSameColor (board!pF) White
                                                              else False 

-- Verifica que no se este moviendo una pieza a un lugar con otra pieza del mismo color
isNotSameColor :: Square -> Player -> Bool
isNotSameColor Nothing _ = True
isNotSameColor (Just(pC,_)) player = pC /= player


-- Verifica si se puede promover una pieza en una coordenada dada
canPromote :: Board -> Position -> Bool
canPromote board p = case board!p of
                        Nothing -> False
                        Just(Black,Pawn) -> isInPromotionZone p Black
                        Just(White,Pawn) -> isInPromotionZone p White
                        Just(Black,Lancer) -> isInPromotionZone p Black
                        Just(White,Lancer) -> isInPromotionZone p White
                        Just(Black,Knight) -> isInPromotionZone p Black
                        Just(White,Knight) -> isInPromotionZone p White
                        Just(Black,Silver) -> isInPromotionZone p Black
                        Just(White,Silver) -> isInPromotionZone p White
                        Just(Black,Rook) -> isInPromotionZone p Black
                        Just(White,Rook) -> isInPromotionZone p White
                        Just(Black,Bishop) -> isInPromotionZone p Black
                        Just(White,Bishop) -> isInPromotionZone p White
                        Just(_,_) -> False

isInPromotionZone :: Position -> Player -> Bool
isInPromotionZone (x,_) Black = (x <= 3) && (x >= 1)
isInPromotionZone (x,_) White = (x <= 9) && (x >= 7)

-- Verifica que el jugador este tratando de mover una pieza de su propio color
canUsePiece :: Board -> Position -> Player -> Bool
canUsePiece board p colour = case board!p of
                                Nothing -> False
                                Just(colourP,_) -> colour == colourP

-- Verifica si se puede posicionar una pieza tomada en una localizacion dada
canDrop :: MonadShogi m => Board -> Position -> Player -> Piece -> m Bool
canDrop board p@(x,y) White piece = case board!p of
                                      Just(_,_) -> return False
                                      Nothing -> case piece of
                                        Lancer -> return (x /= 9)
                                        Knight -> return (x /= 9 && x /= 8)
                                        Pawn -> do noPCheckmate <- noPawnCheckmate board p Black
                                                   return (x /= 9 && not(pawnInLine board y Black) && noPCheckmate)
                                        _ -> return True
canDrop board p@(x,y) Black piece = case board!p of
                                      Just(_,_) -> return False
                                      Nothing -> case piece of
                                        Lancer -> return (x /= 1)
                                        Knight -> return (x /= 1 && x /= 2)
                                        Pawn -> do noPCheckmate <- noPawnCheckmate board p White
                                                   return (x /= 1 && not(pawnInLine board y White) && noPCheckmate)
                                        _ -> return True

-- Verifica si hay algun peon en la linea dada
pawnInLine :: Board -> Int -> Player -> Bool
pawnInLine board y colour = pawnInLine' board (1,y) colour

pawnInLine' :: Board -> Position -> Player -> Bool
pawnInLine' board (9,y) colour = case board!(9,y) of
                                  Nothing -> False
                                  Just (cP,Pawn) -> cP == colour
                                  Just (_,_) -> False
pawnInLine' board (i,y) colour = case board!(i,y) of
                                  Nothing -> pawnInLine' board (y,(i+1)) colour
                                  Just (cP,Pawn) -> if(cP == colour) then True else pawnInLine' board ((i+1),y) colour
                                  Just (_,_) -> pawnInLine' board (y,(i+1)) colour

-- Verifica que no se produzca un jaque mate si se pone el peon en esa posicion
noPawnCheckmate :: MonadShogi m => Board -> Position -> Player -> m Bool
noPawnCheckmate _ (1,_) Black = return True
noPawnCheckmate board p@(x,y) Black = case board!(x-1,y) of
                                            Nothing -> return True
                                            Just(White,King) -> let b' = tryBoardDrop board (Pawn,p) Black
                                                                 in do check <- isCheckMate b' White
                                                                       return (not check)
                                            Just(_,_) -> return True
noPawnCheckmate _ (9,_) White = return True
noPawnCheckmate board p@(x,y) White = case board!(x+1,y) of
                                            Nothing -> return True
                                            Just(Black,King) -> let b' = tryBoardDrop board (Pawn,p) White
                                                                 in do check <- isCheckMate b' Black
                                                                       return (not check)
                                            Just(_,_) -> return True

{- Para optimizar el uso de la funcion isCheck tratar de llamarla solo despues de mover el rey
y hacer una funcion que chequee solo los movimientos legales de la ultima pieza que se movio para ver si
entre esos esta el rey del color opuesto
 -}

-- Verifica si el rey de un color dado esta en jaque fijandose en cada pieza del oponente
isCheck :: Board -> Player -> Bool
isCheck board colour = let (xK,yK) = findKing board colour (1,1)
                        in isCheck' board (xK,yK) (1,1)

-- Mismo comportamiento que isCheck pero ya trabaja con las coordenadas del rey
isCheck' :: Board -> Position -> Position -> Bool
isCheck' board pK (9,9) = if isValidMove board (9,9) pK then True else False
isCheck' board pK (x,9) = if isValidMove board (x,9) pK then True else isCheck' board pK ((x+1),1)
isCheck' board pK (x,y) = if isValidMove board (x,y) pK then True else isCheck' board pK (x,(y+1))

-- Verifica si el ultimo movimiento puso en jaque al rey del oponente
isChecking :: Board -> Player -> Position -> Bool
isChecking b White (x,y) = let (xK,yK) = findKing b Black (1,1)
                            in (b!(xK,yK) /= (Just (White, King))) && isValidMove b (x,y) (xK,yK)
isChecking b Black (x,y) = let (xK,yK) = findKing b White (1,1)
                            in (b!(xK,yK) /= (Just (Black, King))) && isValidMove b (x,y) (xK,yK)

-- Busca el rey de un color dado
findKing :: Board -> Player -> Position -> Position
findKing b c (x,9) = case b!(x,9) of
                      Nothing -> findKing b c ((x+1),1)
                      Just (c',King) -> if (c == c') then (x,9) else findKing b c ((x+1),1)
                      Just (_,_) -> findKing b c ((x+1),1)
findKing b c (x,y) = case b!(x,y) of
                      Nothing -> findKing b c (x,(y+1))
                      Just (c',King) -> if (c == c') then (x,y) else findKing b c (x,(y+1))
                      Just (_,_) -> findKing b c (x,(y+1))

-- Verifica si el rey esta en jaque mate
isCheckMate :: MonadShogi m => Board -> Player -> m Bool
isCheckMate b c = let k = findKing b c (1,1) -- Buscamos el rey
                      (playerPieces,freeSquares) = playersOrFree b c (1,1) -- Obtenemos todas las piezas que se pueden mover o donde se puede dropear
                      in do tp <- getTakenPieces c
                            legalmoves <- allLegalMoves b playerPieces
                            legalDrops <- allLegalDrop b tp c freeSquares
                            return (tryBoardsCheck b k c legalmoves legalDrops)

-- Calcula todas las posiciones donde el jugador tendria sus piezas o donde podria poner piezas ya tomadas
playersOrFree :: Board -> Player -> Position -> ([Position],[Position])
playersOrFree b c p@(9,9) = case b!p of
                            Nothing -> ([],[p])
                            Just (c',_) -> if(c' == c) then ([p],[]) else ([],[])
playersOrFree b c p@(x,9) = let (pl,emp) = playersOrFree b c (x+1,1)
                            in case b!p of
                                  Nothing -> (pl,p:emp)
                                  Just(c',_) -> if(c' == c) then (p:pl,emp) else (pl,emp)
playersOrFree b c p@(x,y) = let (pl,emp) = playersOrFree b c (x,y+1)
                            in case b!p of
                                  Nothing -> (pl,p:emp)
                                  Just(c',_) -> if(c' == c) then (p:pl,emp) else (pl,emp)

-- Funciones de generacion de tableros. Se asume que los movimientos son validos y no se guarda el resultado
-- como el jugador puede terminar haciendo un movimiento distinto. Es solo para descartar jaque mate

-- Aplica el movimiento dado y retorna el tablero resultante
tryBoardMove :: Board -> Move -> Board
tryBoardMove b ((xI,yI),(xF,yF)) = let piece = b!(xI,yI)
                                       b' = setElem Nothing (xI,yI) b
                                    in setElem piece (xF,yF) b'

-- Retorna el tablero resultante de dropear una pieza
tryBoardDrop :: Board -> Drop -> Player -> Board
tryBoardDrop b (piece,pos) c = setElem (Just (c,piece)) pos b

-- Prueba todas las posiciones para ver si en alguna el rey deja de estar en jaque hasta quedarse sin pos
tryBoardsCheck :: Board -> Position -> Player -> [Move] -> [Drop] -> Bool
tryBoardsCheck _ _ _ [] [] = True
tryBoardsCheck b posKing c [] (d:ds) = let b' = tryBoardDrop b d c
                                        in if(isCheck' b' posKing (1,1)) 
                                          then tryBoardsCheck b posKing c [] ds
                                          else False
tryBoardsCheck b posKing c (m:ms) dropPos = let b' = tryBoardMove b m
                                              in if(isCheck' b' posKing (1,1)) 
                                                then tryBoardsCheck b posKing c ms dropPos
                                                else False



-- Funciones para listar los movimientos y drops legales que puede hacer un jugador. 
-- No se le informa al jugador de que puede hacer solo se generan para probar y chequear si hay jaque mate

allLegalMoves :: MonadShogi m => Board -> [Position] -> m [Move]
allLegalMoves _ [] = return []
allLegalMoves b (p:ps) = do ps' <- allLegalMoves b ps
                            return (legalMoves b p ++ ps')

-- Lista todos los movimientos que una pieza en una posicion puede hacer
legalMoves :: Board -> Position -> [Move]
legalMoves b p@(x,y) = case b!p of
                          Nothing -> []
                          Just(Black,Pawn) -> tryMove b p [(x-1,y)]
                          Just(White,Pawn) -> tryMove b p [(x+1,y)]
                          Just(Black,Lancer) -> tryMove b p [(x-1,y),(x-2,y)]
                          Just(White,Lancer) -> tryMove b p [(x+1,y),(x+2,y)]
                          Just(Black,Knight) -> tryMove b p [(x-2,y-1),(x-2,y+1)]
                          Just(White,Knight) -> tryMove b p [(x+2,y-1),(x+2,y+1)]
                          Just(Black,Silver) -> tryMove b p [(x-1,y-1),(x-1,y),(x-1,y+1),(x+1,y-1),(x+1,y+1)]
                          Just(White,Silver) -> tryMove b p [(x-1,y-1),(x-1,y+1),(x+1,y-1),(x+1,y+1),(x+1,y)]
                          Just(_,Bishop) -> tryMove b p (diagMove p)
                          Just(_,Rook) -> tryMove b p (crossMove p)
                          Just(_,King) -> tryMove b p [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y-1),(x+1,y),(x+1,y+1)]
                          Just(_,Horse) -> tryMove b p ((diagMove p) ++ [(x,y-1),(x,y+1),(x-1,y),(x+1,y)])
                          Just(_,Dragon) -> tryMove b p ((crossMove p) ++ [(x-1,y-1),(x-1,y+1),(x+1,y-1),(x+1,y+1)])
                          Just(Black,_) -> tryMove b p [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y)]
                          Just(White,_) -> tryMove b p [(x+1,y+1),(x+1,y),(x+1,y+1),(x,y-1),(x,y+1),(x-1,y)]

-- Filtra cuales de todas las posiciones posibles son validas y elimina las que no
tryMove :: Board -> Position -> [Position] -> [Move]
tryMove _ _ [] = []
tryMove b p (pos:ps) = if(isValidMove b p pos) then (p,pos) : (tryMove b p ps) else (tryMove b p ps)

-- Agrega los movimientos diagonales posibles, descartando los que quedan fuera del tablero
diagMove :: Position -> [Position]
diagMove p = (diag1 p 1) ++ (diag2 p 1) ++ (diag3 p 1) ++ (diag4 p 1)

diag1 :: Position -> Int -> [Position]                
diag1 _ 10 = []
diag1 p@(x,y) i = if ((x-i > 0) && (y-i > 0)) then (x-i,y-i) : diag1 p (i+1) else []

diag2 :: Position -> Int -> [Position]
diag2 _ 10 = []              
diag2 p@(x,y) i = if ((x-i > 0) && (y+i <= 9)) then (x-i,y+i) : diag2 p (i+1) else []

diag3 :: Position -> Int -> [Position]
diag3 _ 10 = []              
diag3 p@(x,y) i = if ((x+i <= 9) && (y-i > 0)) then (x+i,y-i) : diag3 p (i+1) else []

diag4 :: Position -> Int -> [Position]
diag4 _ 10 = []              
diag4 p@(x,y) i = if ((x+i <= 9) && (y+i <= 9)) then (x+i,y+i) : diag4 p (i+1) else []

-- Agrega los movimientos laterales posibles, descartando los que quedan fuera del tablero
crossMove :: Position -> [Position]
crossMove p = (cross1 p 1) ++ (cross2 p 1) ++ (cross3 p 1) ++ (cross4 p 1)

cross1 :: Position -> Int -> [Position]
cross1 _ 10 = []
cross1 p@(x,y) i = if(x-i > 0) then (x-i,y) : cross1 p (i+1) else []

cross2 :: Position -> Int -> [Position]
cross2 _ 10 = []
cross2 p@(x,y) i = if(y-i > 0) then (x,y-i) : cross2 p (i+1) else []

cross3 :: Position -> Int -> [Position]
cross3 _ 10 = []
cross3 p@(x,y) i = if(x+i <= 9) then (x+i,y) : cross3 p (i+1) else []

cross4 :: Position -> Int -> [Position]
cross4 _ 10 = []
cross4 p@(x,y) i = if(y+i <= 9) then (x,y+i) : cross4 p (i+1) else []


allLegalDrop :: MonadShogi m => Board -> TakenPieces -> Player -> [Position] -> m [Drop]
allLegalDrop _ _ _ [] = return []
allLegalDrop b tp c (p:ps) = do d' <- legalDrop b tp c p
                                case d' of
                                  Nothing -> allLegalDrop b tp c ps
                                  Just d -> do ps' <- allLegalDrop b tp c ps
                                               return (d : ps')
-- Prueba si alguna de todas las piezas tomadas se puede poner en una posicion dada 
legalDrop :: MonadShogi m => Board -> TakenPieces -> Player -> Position ->  m (Maybe Drop)
legalDrop _ [] _ _ = return Nothing
legalDrop b (tp:ps) c pos = do d <- canDrop b pos c tp 
                               if(d) then return (Just (tp,pos)) else (legalDrop b ps c pos)
