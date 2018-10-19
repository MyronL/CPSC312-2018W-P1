module CheckersGraphicsBasicPrintLn where


import CheckersData
import Data.Map as Map
import Data.List as List
import System.IO
import Data.Char

import System.Exit
import Control.Concurrent


-- GAME DISPLAY (SIMPLE): BOARD ---------------

-- printBoard prints out the board
printBoard = putStrLn (displayBoard startBoard)

-- display board returns a string representation of the whole board
displayBoard :: GameBoard -> String
displayBoard board = (addRowColLabels (displayBoardHelper board 1 1))

-- displayBoardHelper goes through each tile in the board and displays it
displayBoardHelper :: GameBoard -> Int -> Int -> String
displayBoardHelper board x y
    | y == boardSize+1  = ""
    | x == boardSize    = displayCell board boardSize y ++ (rowLabelRight y) ++ "\n" ++ leftPadding ++ (displayBoardHelper board 1 (y+1))
    | otherwise         = displayCell board x y ++ (displayBoardHelper board (x+1) y)

-- note: also adds row labels 1..8 on right side (Int as y-val)
rowLabelLeft :: Int -> [Char]
rowLabelLeft i = show i ++ " "

rowLabelRight :: Int -> [Char]
rowLabelRight i = "  " ++ show i

-- use this to add padding for the board display
leftPadding = "   "

-- displayCell displays one tile on the board
displayCell :: GameBoard -> Int -> Int -> String
displayCell board x y
    | elem (Square x y) squares = displaySquare board (Square x y)
    | x == 1            = "| "
    | x == boardSize    = " |" 
    | otherwise         = " "

-- displaySquare displays one of the valid squares on the board
displaySquare :: GameBoard -> Square -> String
displaySquare board sq = "|" ++ [displayPlayerPiece (Map.lookup sq board)] ++ "|"

-- displayPlayerPiece displays a piece
displayPlayerPiece :: Maybe Piece -> Char
displayPlayerPiece (Just (Piece pt pl))
  | pl == North = displayNPiece (Piece pt pl) 
  | pl == South = displaySPiece (Piece pt pl)
displayPlayerPiece _ = displayEmpty

-- displayEmpty is a valid square that is currently empty
displayEmpty = '#'

-- displayNPiece and displaySPiece display the pieces for the respective players
displayNPiece :: Piece -> Char
displayNPiece (Piece pt pl) 
  | pt == Starter = 'o'
  | pt == King = '8'

displaySPiece :: Piece -> Char
displaySPiece (Piece pt pl) 
  | pt == Starter = 'x'
  | pt == King = 'K'



-- xIntToStr returns a letter for the given x coordinate
xIntToStr :: Int -> Char
xIntToStr i = chr (96+i)

-- letters is the list of letters to be displayed for the current board size
letters = getLetters boardSize
getLetters :: Int -> String
getLetters 0 = "|"
getLetters n = (getLetters (n-1)) ++ [xIntToStr n] ++ "|"

-- Column Labels: labelling x coord as (a..h) (UI equiv to 1..8)
colLabelTop = "\n" ++ leftPadding ++ letters ++ "\n \n" ++ leftPadding
colLabelBot = "\n" ++ leftPadding ++ letters ++ "\n"

northLabel = "\n" ++ leftPadding ++ "      NORTH      "
southLabel = leftPadding ++ "     SOUTH      \n"

-- addRowColLabels adds the labels around the board
addRowColLabels :: String -> String
addRowColLabels b2s = 
  do 
    northLabel ++ colLabelTop ++ b2s ++ colLabelBot ++ southLabel


instance Show State where
    show (State (GameState board playerType) moves) = displayBoard board ++ "\nCurrent player: " ++ (show playerType) ++ "\nAvailable moves: " ++ (show moves)

instance Show Result where
    show (YourTurn state) = show state

