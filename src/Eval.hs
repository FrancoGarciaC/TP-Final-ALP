module Eval where
import Common
import MonadShogi
import Parser
import Distribution.TestSuite (Result(Error))
import Errors
import Text.Parsec (runParser)
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catch, IOException)
import System.IO
eval :: MonadShogi m => GameActions -> m ()
eval NewG = newGame 
-- Usar catch errors con un handler (aun no definido) en el main y usar throws para indicar lo errores del eval
eval (LoadG name) = do loadedGame <- liftIO $ catch (readFile (name ++ ".txt"))
                                            (\e -> do let err = (e :: IOException) 
                                                      hPutStrLn stderr ("La partida " ++ name ++ "que se quiere cargar no existe")
                                                      return "")
                       let file = parse parseFile loadedGame 
                        in case file of
                          Left e -> failShogi ("Error en el parseo de la partida" ++ name)
                          Right (File (TP tpBlack, TP tpWhite, Game boards)) -> 
                            let player = if even (length boards) then White else Black
                             in loadGame (Game boards) (TP tpBlack, TP tpWhite) player
eval (PlayRec name) = do loadedReplay <- liftIO $ catch (readFile (name ++ ".txt"))
                                                (\e -> do let err = (e :: IOException) 
                                                          hPutStrLn stderr ("El replay " ++ name ++ "que se quiere cargar no existe")
                                                          return "")
                         let file = parse parseFile loadedReplay
                          in case file of
                          Left e -> failShogi ("Error en el parseo del replay" ++ name)
                          Right (File (_, _, Game boards)) -> replayGame (Game boards)
eval _ = failShogi "No implementado"

-- Verifica si se puede realizar el movimiento solicitado
isValidMove :: Board -> Position -> Position -> Bool
isValidMove board pI@(xI,yI) pF@(xF,yF) = if (xI >= 9 or yI >= 9 or xF >= 9 or yF >= 9 or xI < 0 or yI < 0 or xF < 0 or yF < 0 or (xI == xF and yI == yF)) then false
                                    else case board!pI of
                                            Nothing -> false
                                            Just (Black,Pawn) -> if (xI-1 != xF or yI != yF) then false
                                                                 else isNotSameColor (board!pF) Black
                                            Just (White,Pawn) -> if (xI+1 != xF or yI != yF) then false
                                                                 else isNotSameColor (board!pF) White
                                            Just (Black,Lancer) -> if (xI >= xF or yI != yF) then false
                                                                   else isNotSameColor (board!pF) Black
                                            Just (White,Lancer) -> if (xI <= xF or yI != yF) then false
                                                                   else isNotSameColor (board!pF) White
                                            Just (Black,Knight) -> if (yI+1 != yF or yI-1 != yF or xI-2 != xF) then false
                                                                   else isNotSameColor (board!pF) Black
                                            Just (White,Knight) -> if (yI+1 != yF or yI-1 != yF or xI+2 != xF) then false
                                                                   else isNotSameColor (board!pF) White
                                            Just (Black,Silver) -> if ((xI-1 == xF and (yI-1 == yF or yI == yF or yI+1 == yF)) or (xI+1 == xF and (yI-1 == yF or yI+1 == yF)))
                                                                   then isNotSameColor (board!pF) Black
                                                                   else false
                                            Just (White,Silver) -> if ((xI-1 == xF and (yI-1 == yF or yI+1 == yF)) or (xI+1 == xF and (yI-1 == yF or yI == yF or yI+1 == yF)))
                                                                   then isNotSameColor (board!pF) White
                                                                   else false
                                            Just (c,Bishop) -> if (xI == xF or yI == yF) then false
                                                                   else isNotSameColor (board!pF) c
                                            Just (c,Rook) -> if (xI != xf and yI != yF) then false
                                                             else isNotSameColor (board!pF) c
                                            Just (c,King) -> if (xI+1 == xF or xI == xF or xI-1 == xF) and (yI+1 == yF or yI == yF or yI-1 == yF)
                                                             then isNotSameColor (board!pF) c
                                                             else false
                                            Just (c,Dragon) -> if ((xI == xF) or (yI == yF) or (xI-1 == xF and (yI-1 == yF or yI+1 == yF)) or (xI+1 == xF and (yI-1 == yF or yI+1 == yF)))
                                                               then isNotSameColor (board!pF) c
                                                               else false
                                            Just (c,Horse) -> if ((xI != xF) or (yI != yF) or ((xI+1 == xF or xI-1 == xF) and yI == yF) or (xI == xF and (yI-1 == yF or yI+1 == yF)))
                                                              then isNotSameColor (board!pF) c
                                                              else false
                                            Just (Black,_) -> if ((xI-1 == xF and (yI-1 == yF or yI == yF or yI+1 == yF)) or (xI == xF and (yI-1 == yF or yI+1 == yF)) or (xI+1 == xF and yI == yF))
                                                              then isNotSameColor (board!pF) Black
                                                              else false
                                            Just (White,_) -> if ((xI-1 == xF and yI == yF) or (xI == xF and (yI-1 == yF or yI+1 == yF)) or (xI+1 == xF and (yI-1 == yF or yI == yF or yI+1 == yF)))
                                                              then isNotSameColor (board!pF) White
                                                              else false 

-- Verifica que no se este moviendo una pieza a un lugar con otra pieza del mismo color
isNotSameColor :: Square -> Player -> Bool
isNotSameColor Nothing _ = true
isNotSameColor Just(pC,_) player = pC != player


-- Verifica si se puede promover una pieza en una coordenada dada
canPromote :: Board -> Position -> Bool
canPromote board p@(x,y) = case board!p of
                            Nothing -> false
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
                            Just(_,_) -> false

isInPromotionZone :: Position -> Player -> Bool
isInPromotionZone (x,_) Black = x < 3 and x >= 0
isInPromotionZone (x,_) White = x < 9 and x >= 6

-- Verifica que el jugador este tratando de mover una pieza de su propio color
canUsePiece :: Board -> Position -> Player -> Bool
canUsePiece board p@(x,y) colour = case board!p of
                                    Nothing -> false
                                    Just(colourP,_) -> colour == colourP

-- Verifica si se puede posicionar una pieza tomada en una localizacion dada
canDrop :: Board -> Position -> Player -> Piece -> Bool
canDrop board p@(x,y) Black piece = case board!p of
                                      Just(_,_) -> false
                                      Nothing -> case piece of
                                        Lancer -> y != 8
                                        Knight -> y != 8 and y != 7
                                        Pawn -> y != 8 and not(pawnInLine board p player) and (noPawnCheckmate board p player)
canDrop board p@(x,y) White piece = case board!p of
                                      Just(_,_) -> false
                                      Nothing -> case piece of
                                        Lancer -> y != 0
                                        Knight -> y != 0 and y != 1
                                        Pawn -> y != 0 and not(pawnInLine board p player) and (noPawnCheckmate board p player)

-- Verifica si hay algun peon en la linea dada
pawnInLine :: Board -> Position -> Player -> Bool
pawnInLine board p colour = pawnInLine' board p 1

pawnInLine' :: Board -> Position -> Player -> Integer -> Bool
pawnInLine' board p 9 colour = case board!p of
                                     Nothing -> false
                                     Just (cP,Pawn) -> cP == colour
                                     Just (_,_) -> false
pawnInLine' board p i colour = case board!p of
                                     Nothing -> pawnInLine' board p (i+1) colour
                                     Just (cP,Pawn) -> if(cP == colour) then true else pawnInLine' board p (i+1) colour
                                     Just (_,_) -> pawnInLine' board p (i+1) colour

-- Verifica que no se produzca un jaque mate si se pone el peon en esa posicion
noPawnCheckmate :: Board -> Position -> Player -> Bool
noPawnCheckmate board p@(x,y) Black = case board!(x+1,y) of
                                        Nothing -> true
                                        -- Se necesita definir la funcion que chequee que se este en un jaque mate


{- Para optimizar el uso de la funcion isCheck tratar de llamarla solo despues de mover el rey
y hacer una funcion que chequee solo los movimientos legales de la ultima pieza que se movio para ver si
entre esos esta el rey del color opuesto
 -}

-- Verifica si el rey de un color dado esta en jaque fijandose en cada pieza del oponente
isCheck :: Board -> Player -> Bool
isCheck board colour = let (xK,yK) = findKing board player 1 1
                        in isCheck' board colour (xk,yK) 1 1

-- Mismo comportamiento que isCheck pero ya trabaja con las coordenadas del rey
isCheck' :: Board -> Player -> Position -> Integer -> Integer -> Bool
isCheck' board colour pK 8 8 = if isValidMove board (8,8) pk then true else false
isCheck' board colour pK x 9 = if isValidMove board (x,8) pk then true else isCheck' board colour pK (x+1) 1
isCheck' board colour pK x y = if isValidMove board (x,y) pk then true else isCheck' board colour pK x (y+1)

-- Verifica si el ultimo movimiento puso en jaque al rey del oponente
isChecking :: Board -> Player -> Position -> Bool
isChecking b White (x,y) = let (xK,yK) = findKing board Black 0 0
                            in isValidMove b (x,y) (xK,yK)
isChecking b Black (x,y) = let (xK,yK) = findKing board White 0 0
                            in isValidMove b (x,y) (xK,yK)

-- Busca el rey de un color dado
findKing :: Board -> Player -> Integer -> Integer -> Position
findKing b c x 8 = case board!(x,8) of
                      Nothing -> findKing b c (x+1) 0
                      Just (c,King) -> (x,8)
                      Just (_,_) -> findKing b c (x+1) 0
findKing b c x y = case board!(x,y) of
                      Nothing -> findKing b c x (y+1)
                      Just (c,King) -> (x,y)
                      Just (_,_) -> findKing b c x (y+1)

-- Verifica si el rey esta en jaque mate
isCheckMate :: Board -> TakenPieces -> Player -> Bool
isCheckMate b tp c = let (xK,yK) = findKing b c 0 0 -- Buscamos el rey
                         (playerPieces,freeSquares) = PlayersOrFree b c (0,0)
                      in 

-- Calcula todas las posiciones donde el jugador tendria sus piezas o donde podria poner piezas ya tomadas
PlayersOrFree :: Board -> Player -> Position -> ([Position],[Position])
PlayersOrFree b c p@(8,8) = case b!p of
                            Nothing -> ([],p)
                            Just (c',_) -> if(c' == c) then (p,[]) else ([],[])
PlayersOrFree b c p@(x,8) = let (pl,emp) = PlayersOrFree b c (x+1,0)
                            case b!p of
                              Nothing -> (pl,p:emp)
                              Just(c',_) -> if(c' == c) then (p:pl,emp)
PlayersOrFree b c p@(x,y) = let (pl,emp) = PlayersOrFree b c (x,y+1)
                            case b!p of
                              Nothing -> (pl,p:emp)
                              Just(c',_) -> if(c' == c) then (p:pl,emp)

-- Aplica el movimiento dado y retorna el tablero resultante
tryBoardMove :: Board -> Move -> Board
tryBoardMove b m@(I@(xI,yI),F@(xF,yF)) = do piece <- b!I
                                            

-- Lista todos los movimientos que una pieza en una posicion puede hacer
legalMoves :: Board -> Position -> [Moves]
legalMoves b p@(x,y) -> case board!p of
                          Nothing -> []
                          Just(Black,Pawn) -> tryMove b p [(x-1,y)]
                          Just(White,Pawn) -> tryMove b p [(x+1,y)]
                          Just(Black,Lancer) -> tryMove b p [(x-1,y),(x-2,y)]
                          Just(White,Lancer) -> tryMove b p [(x+1,y),(x+2,y)]
                          Just(Black,Knight) -> tryMove b p [(x-2,y-1),(x-2,y+1)]
                          Just(White,Knight) -> tryMove b p [(x+2,y-1),(x+2,y+1)]
                          Just(Black,Silver) -> tryMove b p [(x-1,y-1),(x-1,y),(x-1,y+1),(x+1,y-1),(x+1,y+1)]
                          Just(White,Silver) -> tryMove b p [(x-1,y-1),(x-1,y+1),(x+1,y-1),(x+1,y+1),(x+1,y)]
                          Just(c,Bishop) -> tryMove b p (diagMove p)
                          Just(c,Rook) -> tryMove b p (crossMove p)
                          Just(c,King) -> tryMove b p [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y-1),(x+1,y),(x+1,y+1)]
                          Just(c,Horse) -> tryMove b p ((diagMove p) ++ [(x,y-1),(x,y+1),(x-1,y),(x+1,y)])
                          Just(c,Dragon) -> tryMove b p ((crossMove p) ++ [(x-1,y-1),(x-1,y+1),(x+1,y-1),(x+1,y+1)])
                          Just(Black,_) -> tryMove b p [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y-1),(x,y+1)(x+1,y)]
                          Just(White,_) -> tryMove b p [(x+1,y+1),(x+1,y),(x+1,y+1),(x,y-1),(x,y+1)(x-1,y)]

-- Filtra cuales de todas las posiciones posibles son validas y elimina las que no
tryMove :: Board -> Position -> [Position] -> [Moves]
tryMove _ _ [] = []
tryMove b p@(x,y) (x:xs) = if(isValidMove b p x) then (p,x) : (isValidMove b p xs) else (isValidMove b p xs)

-- Agrega los movimientos diagonales posibles, descartando los que quedan fuera del tablero
diagMove :: Position -> [Position]
diagMove p = (diag1 p 1) ++ (diag2 p 1) ++ (diag3 p 1) ++ (diag4 p 1)
                
diag1 _ 9 = []
diag1 p@(x,y) i = if (x-i >= 0 and y-i >= 0) then (x-i,y-i) : diag1 p (i+1) else []

diag2 _ 9 = []              
diag2 p@(x,y) i = if (x-i >= 0 and y+i <= 8) then (x-i,y+i) : diag2 p (i+1) else []

diag3 _ 9 = []              
diag3 p@(x,y) i = if (x+i <= 8 and y-i >= 0) then (x+i,y-i) : diag3 p (i+1) else []

diag4 _ 9 = []              
diag4 p@(x,y) i = if (x+i <= 8 and y+i <= 8) then (x+i,y+i) : diag4 p (i+1) else []

-- Agrega los movimientos laterales posibles, descartando los que quedan fuera del tablero
crossMove :: Position -> [Position]
crossMove p = (cross1 p 1) ++ (cross2 p 1) ++ (cross3 p 1) ++ (cross4 p 1)

cross1 _ 9 = []
cross1 p@(x,y) i = if(x-i >= 0) then (x-i,y) : cross1 p (i+1) else []

cross2 _ 9 = []
cross2 p@(x,y) i = if(y-i >= 0) then (x,y-i) : cross2 p (i+1) else []

cross3 _ 9 = []
cross3 p@(x,y) i = if(x+i <= 8) then (x+i,y) : cross3 p (i+1) else []

cross4 _ 9 = []
cross4 p@(x,y) i = if(y+i <= 8) then (x,y+i) : cross4 p (i+1) else []

-- Lista todas las posiciones donde se puede poner al menos una pieza
legalDrop :: Board -> TakenPieces -> [Drop]
legalDrop b tp = legalDrop' b tp (0,0)

legalDrop' :: Board -> TakenPieces -> Player -> Position -> [Drop]
legalDrop' b tp c p@(8,8) = case tryDrop b p c tp of
                                  Nothing -> []
                                  Just p' -> p'
legalDrop' b tp c p@(x,8) = case tryDrop b p c tp of 
                                  Nothing -> legalDrop' b tp c (x+1,0)
                                  Just p' -> p' : (legalDrop' b tp c (x+1,0))
legalDrop' b tp c p@(x,y) = case tryDrop b p c tp of
                                  Nothing -> legalDrop' b tp c (x,y+1)
                                  Just p' -> p' : (legalDrop' b tp c (x,y+1)) 

-- Prueba si alguna de todas las piezas tomadas se puede poner en una posicion dada 
tryDrop :: Board -> Position -> Player -> [Piece] -> Maybe Drop
tryDrop _ _ _ [] = Nothing
tryDrop b pos@(x,y) c (p:ps) = if(canDrop b pos c p) then Just (p,pos) else (tryDrop b pos c ps)