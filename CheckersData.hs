<<<<<<< HEAD
=======
module CheckersData where
>>>>>>> 5da0998b59c55f6407a64c4a6512ce8752a729ac

import Data.Map as Map
import Data.List as List



<<<<<<< HEAD
data Player = North
  | South
  deriving (Eq)  

data PieceType = Starter
  | King
  deriving (Eq)
  
data Piece = Piece PieceType Player
  | Empty
  deriving (Eq)
=======
data PlayerType = North | South
  deriving (Eq, Show)

data PieceType = Starter | King
  deriving (Eq, Show)
  
data Piece = Piece PieceType PlayerType
  | Empty
  deriving (Eq, Show)
>>>>>>> 5da0998b59c55f6407a64c4a6512ce8752a729ac



-- SINGLE SQUARE TILE IN A CHECKERS BOARD
data Square = Square Int Int
<<<<<<< HEAD
=======
    deriving (Show)
>>>>>>> 5da0998b59c55f6407a64c4a6512ce8752a729ac

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



<<<<<<< HEAD
data GameBoard = GameBoard (Map Square Piece) 
=======
type GameBoard = Map Square Piece
>>>>>>> 5da0998b59c55f6407a64c4a6512ce8752a729ac



-- Current Game State:
-- - Gameboard
-- - Player's turn
-- - List of legal moves
-- - Player's action
<<<<<<< HEAD
data GameState = GameState GameBoard Player [Move] Action
=======
data InternalState = GameState GameBoard Player [Move] Action

data State = State InternalState [Action]  -- internal_state available_actions

data Result = EndOfGame Double State    -- end of game, value, starting state
            | ContinueGame State        -- continue with new state

type Game = Action -> State -> Result

type Player = State -> Action
>>>>>>> 5da0998b59c55f6407a64c4a6512ce8752a729ac


-- GAME INIT ---------------------------



-- All Steppable Checkers Squares: 
--    (2,1), (4,1) ... 
--    (1,2), (3,2) ...
<<<<<<< HEAD
squares = [(Square x y) | x <- [1..8], y <- [1..8], (mod x 2 == 0 && mod y 2 == 1) || (mod x 2 == 1 && mod y 2 == 0)]


startGame = initSquares (GameBoard (Map.empty)) squares
=======
squares = [(Square x y) | y <- [1..8], x <- [1..8], (mod x 2 == 0 && mod y 2 == 1) || (mod x 2 == 1 && mod y 2 == 0)]


startBoard = initSquares Map.empty squares
>>>>>>> 5da0998b59c55f6407a64c4a6512ce8752a729ac



initSquares :: GameBoard -> [Square] -> GameBoard 
<<<<<<< HEAD
initSquares (GameBoard m) [] = (GameBoard m)
initSquares (GameBoard m) (h:t) = initSquares (initPiece (GameBoard m) h $ initPieceAtSquare h) t
=======
initSquares board [] = board
initSquares board (h:t) = initSquares (initPiece board h $ initPieceAtSquare h) t
>>>>>>> 5da0998b59c55f6407a64c4a6512ce8752a729ac

initPieceAtSquare :: Square -> Piece
initPieceAtSquare (Square x y) 
  | y <= 3 = (Piece Starter North)
  | y >= 6 = (Piece Starter South)
  | otherwise = Empty


initPiece :: GameBoard -> Square -> Piece -> GameBoard
<<<<<<< HEAD
initPiece (GameBoard b) sq piece = GameBoard $ Map.insert sq piece b
=======
initPiece board sq piece =  Map.insert sq piece board
>>>>>>> 5da0998b59c55f6407a64c4a6512ce8752a729ac



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
<<<<<<< HEAD
    " | |x| |x| |x| |x| " ] 

displayBoard :: GameBoard -> [[Char]]
displayBoard (GameBoard b) = arrboard


displayRow :: Int -> [Square] -> [Char]
displayRow n [] = ""
displayRow n (h:t) = 'A' : displayRow n t

displaySquare :: Square -> [Char]
displaySquare (Square x y) = ""


displayPlayerPiece :: Piece -> [Char] 
displayPlayerPiece (Piece pt pl) 
  | pl == North = displayNPiece (Piece pt pl) 
  | pl == South = displaySPiece (Piece pt pl) 
  | otherwise = displayEmpty

displayPieceAtSquare :: GameBoard -> Square -> [Char]
displayPieceAtSquare (GameBoard m) (Square x y) = ""



displayNPiece :: Piece -> [Char] 
displayNPiece (Piece pt pl) 
  | pt == Starter = "o"
  | pt == King = "8"
  | otherwise = ""

displaySPiece :: Piece -> [Char] 
displaySPiece (Piece pt pl) 
  | pt == Starter = "x"
  | pt == King = "K"
  | otherwise = ""

displayEmpty = "#"
displayWhiteSquare = " "
  
  
  
displaySquares :: GameBoard -> [Square] -> [Char]
displaySquares (GameBoard m) [] = ""
=======
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

>>>>>>> 5da0998b59c55f6407a64c4a6512ce8752a729ac
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


<<<<<<< HEAD
main = print $ "OK"
=======
main = print $ "OK"
>>>>>>> 5da0998b59c55f6407a64c4a6512ce8752a729ac
