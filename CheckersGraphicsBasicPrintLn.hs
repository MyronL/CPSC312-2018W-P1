module CheckersGraphicsBasicPrintLn where


import CheckersData
import Data.Map as Map
import Data.List as List
import System.IO

import Data.List.Split 


-- GAME DISPLAY (SIMPLE): BOARD ---------------

-- note: also adds row labels 1..8 on right side (Int as y-val)
rowLabelLeft :: Int -> [Char]
rowLabelLeft i = show i ++ " "

rowLabelRight :: Int -> [Char]
rowLabelRight i = " " ++ show i

displayBoardHelper :: GameBoard -> Int -> Int -> String
displayBoardHelper board x y
    | y == boardSize+1  = ""
    | x == boardSize    = displayCell board boardSize y ++ (rowLabelRight y) ++ "\n" ++ (displayBoardHelper board 1 (y+1))
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
displayBoard board = displayBoardHelper board 1 1

printBoard = putStrLn (addRowColLabels (displayBoard startBoard))


-- Column Labels: labelling x coord as (a..h) (UI equiv to 1..8)
colLabelTop = "\n|a|b|c|d|e|f|g|h|\n \n"
colLabelBot = "\n|a|b|c|d|e|f|g|h|\n"

addRowColLabels :: String -> String
addRowColLabels b2s = 
  do 
    colLabelTop ++ b2s ++ colLabelBot


-- helper replace function
--replace :: Eq a => a -> a -> [a] -> [a]
--replace a b = List.map $ \c -> if c == a then b else c


-- GAME DISPLAY (SIMPLE): SCREEN & USER DIALOGUE ---------------

-- WELCOME SCREEN (pending)
welcomeScreen = ["WELCOME TO HASKELL CHECKERS",
    "Play at your own risk..."]
-- 
startOptions = ["Commands:", 
    "Player VS Player (enter 1)", 
    "Player VS AI CPU (enter 2)", 
    "Exit (enter any other key)"]


-- todo:
displayPlayerTurn :: PlayerType -> Turn -> [Char]
displayPlayerTurn p t =  "PLAYER " ++ show p ++ " TURN " ++ show t ++ ": GO" 
