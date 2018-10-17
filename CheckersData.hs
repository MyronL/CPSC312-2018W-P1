module CheckersData where

import Data.Map as Map
import Data.List as List



data PlayerType = North | South
  deriving (Eq, Show)

data PieceType = Starter | King
  deriving (Eq, Show)
  
data Piece = Piece PieceType PlayerType
  | Empty
  deriving (Eq, Show)



-- SINGLE SQUARE TILE IN A CHECKERS BOARD
data Square = Square Int Int
    deriving (Show)

instance Eq Square where
    (Square a b) == (Square c d) = a == c && b == d

instance Ord Square where
    (Square a b) <= (Square c d) = a < c || (a == c && b <= d)





data Move = Jump Square [Square]
  | Forward Square
  | Backward Square

data Action = Move
  | Emote
  | Concede



type GameBoard = Map Square Piece



-- Current Game State:
-- - Gameboard
-- - Player's turn
-- - List of legal moves
-- - Player's action
data InternalState = GameState GameBoard Player [Move] Action

data State = State InternalState [Action]  -- internal_state available_actions

data Result = EndOfGame Double State    -- end of game, value, starting state
            | ContinueGame State        -- continue with new state

type Game = Action -> State -> Result

type Player = State -> Action


-- GAME INIT ---------------------------



-- All Steppable Checkers Squares: 
--    (2,1), (4,1) ... 
--    (1,2), (3,2) ...
squares = [(Square x y) | y <- [1..8], x <- [1..8], (mod x 2 == 0 && mod y 2 == 1) || (mod x 2 == 1 && mod y 2 == 0)]


startBoard = initSquares Map.empty squares



initSquares :: GameBoard -> [Square] -> GameBoard 
initSquares board [] = board
initSquares board (h:t) = initSquares (initPiece board h $ initPieceAtSquare h) t

initPieceAtSquare :: Square -> Piece
initPieceAtSquare (Square x y) 
  | y <= 3 = (Piece Starter North)
  | y >= 6 = (Piece Starter South)
  | otherwise = Empty


initPiece :: GameBoard -> Square -> Piece -> GameBoard
initPiece board sq piece =  Map.insert sq piece board



-- GAME DISPLAY (SIMPLE) ---------------


-- DO: RECREATE
file = 
  do
    print $ " |o| |o| |o| |o| | "
    print $ " | |o| |o| |o| |o| "
    print $ " |o| |o| |o| |o| | "
    print $ " | |#| |#| |#| |#| "
    print $ " |#| |#| |#| |#| | "
    print $ " | |x| |x| |x| |x| "
    print $ " |x| |x| |x| |x| | "
    print $ " | |x| |x| |x| |x| "


arrboard =
  [ " |o| |o| |o| |o| | ",
    " | |o| |o| |o| |o| ",
    " |o| |o| |o| |o| | ",
    " | |#| |#| |#| |#| ",
    " |#| |#| |#| |#| | ",
    " | |x| |x| |x| |x| ",
    " |x| |x| |x| |x| | ",
    " | |x| |x| |x| |x| " ]

displayBoardHelper board squares _ 9 = ""
displayBoardHelper board squares 8 y = displayCell board squares 8 y ++ "\n" ++ (displayBoardHelper board squares 1 (y+1))
displayBoardHelper board squares x y = displayCell board squares x y ++ (displayBoardHelper board squares (x+1) y)

displayCell :: GameBoard -> [Square] -> Int -> Int -> String
displayCell board squares x y
    | elem (Square x y) squares = displaySquare board (Square x y)
    | x == 1                    = "| "
    | x == 8                    = " |"
    | otherwise                 = " "


displayRow :: GameBoard -> Int -> [Square] -> String
displayRow board n [] = ""
displayRow board n ((Square x y):t)
    | y == n    = displaySquare board (Square x y) ++ (displayRow board n t)
    | otherwise = (displayRow board n t)

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
  
  
  
displayBoard :: GameBoard -> [Square] -> String
displayBoard board squares = displayBoardHelper board squares 1 1

printBoard = putStrLn (displayBoard startBoard squares)

--displaySquares (GameBoard m) (h:t) = displaySquares (displaySquare (GameBoard m) h $ displayPlayerPiece h) t

--displaySquares (GameBoard m) (h:t) = displaySquares (displayPieceAtSquare (GameBoard m) h $ displayPlayerPiece h) t  

--initSquares (GameBoard m) (h:t) = initSquares (initPiece (GameBoard m) h $ initPieceAtSquare h) t

  
{-

" |o| |o| |o| |o| | "
" | |o| |o| |o| |o| "
" |o| |o| |o| |o| | "
" | |#| |#| |#| |#| "
" |#| |#| |#| |#| | "
" | |x| |x| |x| |x| "
" |x| |x| |x| |x| | "
" | |x| |x| |x| |x| "

" |o|#|o|#|o|#|o|#| "
" |#|o|#|o|#|o|#|o| "
" |o|#|o|#|o|#|o|#| "
" |#| |#| |#| |#| | "
" | |#| |#| |#| |#| "
" |#|x|#|x|#|x|#|x| "
" |x|#|x|#|x|#|x|#| "
" |#|x|#|x|#|x|#|x| "

-}


main = print $ "OK"
