module Parser where

import Common
import Text.Parsec (eof)
import Text.ParserCombinators.Parsec hiding (parse)
import Text.Parsec.Token
import Text.Parsec.Language ( emptyDef )
import Data.Matrix (fromLists, (!))
-- Testing function
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

--Parsing analizer 
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , reservedNames   = ["King", "K"
                        , "Rook", "R"
                        , "Dragon", "D"
                        , "Bishop", "B"
                        , "Horse", "H"
                        , "Golden", "G"
                        , "Silver", "S"
                        , "PromotedSilver", "PS"
                        , "Knight", "Kn"
                        , "PromotedKnight", "PK"
                        , "Lancer", "L"
                        , "PromotedLancer", "PL"
                        , "Pawn", "P"
                        , "PromotedPawn", "PP"
                        , "Black", "White"]
    , reservedOpNames = ["ShowBoard", "s"
                        , "Move", "m"
                        , "Drop", "d"
                        , "Promote", "p"
                        , "Save", "sg"
                        , "New", "n"
                        , "Load", "lg"
                        , "Record", "r"]
    }
  )


pieceParser :: Parser Piece
pieceParser = do (reservedOp lis  "King" <|> reservedOp lis "k")
                 return King
                <|> do (reservedOp lis "Rook" <|> reservedOp lis "r")
                       return Rook
                      <|> do (reservedOp lis "Dragon" <|> reservedOp lis "d")
                             return Dragon
                            <|> do (reservedOp lis "Bishop" <|> reservedOp lis "b")
                                   return Bishop
                                  <|> do (reservedOp lis "Horse" <|> reservedOp lis "h")
                                         return Horse
                                        <|> do (reservedOp lis "Golden" <|> reservedOp lis "g")
                                               return Golden
                                              <|> do (reservedOp lis "Silver" <|> reservedOp lis "s")
                                                     return Silver
                                                    <|> do (reservedOp lis "PromotedSilver" <|> reservedOp lis "ps")
                                                           return PromotedSilver
                                                          <|> do (reservedOp lis "Knight" <|> reservedOp lis "kn")
                                                                 return Knight
                                                                <|> do (reservedOp lis "PromotedKnight" <|> reservedOp lis "pk")
                                                                       return PromotedKnight
                                                                      <|> do (reservedOp lis "Lancer" <|> reservedOp lis "l")
                                                                             return Lancer
                                                                            <|> do (reservedOp lis "PromotedLancer" <|> reservedOp lis "pl")
                                                                                   return PromotedLancer
                                                                                  <|> do (reservedOp lis "Pawn" <|> reservedOp lis "p")
                                                                                         return Pawn
                                                                                        <|> do (reservedOp lis "PromotedPawn" <|> reservedOp lis "pp")
                                                                                               return PromotedPawn

parsePos :: Parser Position
parsePos = do symbol lis "("
              p1 <- natural lis
              comma lis
              p2 <- natural lis
              symbol lis ")"
              return (fromInteger p1, fromInteger p2)

parseFileName :: Parser FileName
parseFileName = do s <- many alphaNum
                   return s

parsePlayer :: Parser Player
parsePlayer = do reservedOp lis "Black"
                 return Black
                <|> do reservedOp lis "White"
                       return White

parseTakenPieces :: Parser TakenPieces
parseTakenPieces = do pieces <- between (symbol lis "[") (symbol lis "]") (sepBy pieceParser (symbol lis ","))
                      return pieces

parseElem :: Parser Square
parseElem = do symbol lis "("
               player <- parsePlayer
               comma lis
               piece <- pieceParser
               symbol lis ")"
               return  (Just (player,piece))
               <|> do symbol lis "_"
                      return Nothing

parseRow :: Parser [Square]
parseRow = do symbol lis "["
              f <- parseElem
              squares <- many (do { comma lis ; parseElem })
              symbol lis "]"
              return (f : squares)

parseBoard :: Parser Board
parseBoard = do rows <- between (symbol lis "[\n") (symbol lis "]\n") (many parseRow)
                return (fromLists rows)

parseGame :: Parser Game
parseGame = do moves <- manyTill parseBoard (try (string "-\n"))
               return moves

-- Parseara solo las acciones que determinan el estado inicial de la monada
parseStartAction :: Parser StartAction
parseStartAction = do (reservedOp lis "n" <|> reservedOp lis "New")
                      return NewG
                     <|> do (reservedOp lis "l" <|> reservedOp lis "Load")
                            LoadG <$> parseFileName
                           <|> do (reservedOp lis "r" <|> reservedOp lis "Record")
                                  PlayRec <$> parseFileName

-- Parseara todas las acciones que se pueden hacer una vez que se esta jugando
parseGameAction :: Parser GameActions
parseGameAction = do (reservedOp lis "sb" <|> reservedOp lis "ShowBoard")
                     return ShowB
                     <|> do (reservedOp lis "sp" <|> reservedOp lis "ShowTakenPieces")
                            return ShowT
                           <|> do (reservedOp lis "m" <|> reservedOp lis "Move")
                                  pos1 <- parsePos
                                  pos2 <- parsePos
                                  return (MoveP (pos1,pos2))
                                 <|> do (reservedOp lis "d" <|> reservedOp lis "Drop")
                                        p <- pieceParser
                                        pos <- parsePos
                                        return (DropP (p,pos))
                                       <|> do (reservedOp lis "p" <|> reservedOp lis "Promote")
                                              PromP <$> parsePos
                                             <|> do (reservedOp lis "sg" <|> reservedOp lis "Save")
                                                    SaveG <$> parseFileName


parseFile :: Parser File
parseFile = do game <- parseGame
               tpBlack <- parseTakenPieces
               tpWhite <- parseTakenPieces
               checkAndWin <- manyTill parseBool eof
              --  win <- parseBool
              -- eof
               return (File (tpBlack, tpWhite, game, checkAndWin!!0, checkAndWin!!1))
              --  return (File ([],[],game,False,False))

parseBool :: Parser Bool
parseBool = do (symbol lis "si" <|> symbol lis "s" <|> symbol lis "yes" <|> symbol lis "y" <|> symbol lis "True")
               return True
               <|> do (symbol lis "no" <|> symbol lis "n" <|> symbol lis "False")
                      return False
               

parse :: Parser a -> String -> Either ParseError a
parse p = runParser p () ""

mainActionParser :: String -> Either ParseError StartAction
mainActionParser = parse (try $ totParser parseStartAction)

gameActionParser :: String -> Either ParseError GameActions
gameActionParser = parse (try $ totParser parseGameAction)

parseConfirmation :: String -> Either ParseError Bool
parseConfirmation = parse (totParser parseBool)

fileParser :: String -> Either ParseError File
fileParser = parse (try $ totParser parseFile)

-- Deparsers to save the game
deparsePiece :: Square -> String
deparsePiece Nothing = "_"
deparsePiece (Just (c,p)) = "(" ++ show c ++ "," ++ show p ++ ")"

deparseBoard :: Board -> String
deparseBoard b = "[\n" ++ deparseBoard' b (1,1) ++ "]"

deparseBoard' :: Board -> Position -> String
deparseBoard' b p@(9,9) = "," ++ deparsePiece (b!p) ++ "]\n"
deparseBoard' b p@(x,9) = "," ++ deparsePiece (b!p) ++ "]\n" ++ deparseBoard' b (x+1,1)
deparseBoard' b p@(x,1) = "[" ++ deparsePiece (b!p) ++ deparseBoard' b (x,2)
deparseBoard' b p@(x,y) = "," ++ deparsePiece (b!p) ++ deparseBoard' b (x,y+1)

deparseTakenPieces :: TakenPieces -> String
deparseTakenPieces xs = "[" ++ deparseTakenPieces' xs

deparseTakenPieces' :: TakenPieces -> String
deparseTakenPieces' [] = "]"
deparseTakenPieces' [x] = show x ++ deparseTakenPieces' []
deparseTakenPieces' (x:xs) = show x ++ "," ++ deparseTakenPieces' xs

deparseCheck :: Bool -> String
deparseCheck False = "False"
deparseCheck True = "True"