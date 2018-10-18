module CheckersGraphicsBasicPrintLn where


import CheckersData
import Data.Map as Map
import Data.List as List
import System.IO

import System.Exit
import Control.Concurrent


-- GAME DISPLAY (SIMPLE): BOARD ---------------

-- note: also adds row labels 1..8 on right side (Int as y-val)
rowLabelLeft :: Int -> [Char]
rowLabelLeft i = show i ++ " "

rowLabelRight :: Int -> [Char]
rowLabelRight i = "  " ++ show i

-- use this to add padding for the board display
leftPadding = "   "

displayBoardHelper :: GameBoard -> Int -> Int -> String
displayBoardHelper board x y
    | y == boardSize+1  = ""
    | x == boardSize    = displayCell board boardSize y ++ (rowLabelRight y) ++ "\n" ++ leftPadding ++ (displayBoardHelper board 1 (y+1))
    | otherwise         = displayCell board x y ++ (displayBoardHelper board (x+1) y)

displayCell :: GameBoard -> Int -> Int -> String
displayCell board x y
    | elem (Square x y) squares = displaySquare board (Square x y)
    | x == 1            = "| "
    | x == boardSize    = " |" 
    | otherwise         = " "

displaySquare :: GameBoard -> Square -> String
displaySquare board sq = "|" ++ [displayPlayerPiece (Map.lookup sq board)] ++ "|"


displayPlayerPiece :: Maybe Piece -> Char
displayPlayerPiece (Just (Piece pt pl))
  | pl == North = displayNPiece (Piece pt pl) 
  | pl == South = displaySPiece (Piece pt pl)
displayPlayerPiece _ = displayEmpty



displayNPiece :: Piece -> Char
displayNPiece (Piece pt pl) 
  | pt == Starter = 'o'
  | pt == King = '8'

displaySPiece :: Piece -> Char
displaySPiece (Piece pt pl) 
  | pt == Starter = 'x'
  | pt == King = 'K'

displayEmpty = '#'
displayWhiteSquare = ' '



displayBoard :: GameBoard -> String
displayBoard board = (addRowColLabels (displayBoardHelper board 1 1))

printBoard = putStrLn (displayBoard startBoard)


-- Column Labels: labelling x coord as (a..h) (UI equiv to 1..8)
colLabelTop = "\n" ++ leftPadding ++ "|a|b|c|d|e|f|g|h|\n \n" ++ leftPadding 
colLabelBot = "\n" ++ leftPadding ++ "|a|b|c|d|e|f|g|h|\n"

northLabel = "\n" ++ leftPadding ++ "|---- NORTH ----|"
southLabel = leftPadding ++ "|---- SOUTH ----|\n"

addRowColLabels :: String -> String
addRowColLabels b2s = 
  do 
    northLabel ++ colLabelTop ++ b2s ++ colLabelBot ++ southLabel




instance Show State where
    show (State (GameState board playerType) moves) = displayBoard board ++ "\nCurrent player: " ++ (show playerType) ++ "\nAvailable moves: " ++ (show moves)

instance Show Result where
    show (YourTurn state) = show state

