module CheckersData where

import Data.Map as Map
import Data.List as List
import System.IO

-- To run it, try:
-- ghci
-- :load TwentyQs
-- go


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


type GameBoard = Map Square Piece



-- Current Game State:
-- - Gameboard
-- - Player's turn
-- - List of legal moves
-- - Player's action
data InternalState = GameState GameBoard PlayerType

data State = State InternalState [Action]  -- internal_state available_actions

data Result = EndOfGame Double State    -- end of game, value, starting state
            | ContinueGame State        -- continue with new state

type Game = Action -> State -> Result

type Player = State -> Action


-- GAME INIT ---------------------------


boardSize = 8

-- All Steppable Checkers Squares: 
--    (2,1), (4,1) ... 
--    (1,2), (3,2) ...
squares = [(Square x y) | y <- [1..boardSize], x <- [1..boardSize], (mod x 2 == 0 && mod y 2 == 1) || (mod x 2 == 1 && mod y 2 == 0)]


startBoard = initSquares Map.empty squares



initSquares :: GameBoard -> [Square] -> GameBoard
initSquares board [] = board
initSquares board (h:t) = initSquares (initPiece board h $ initPieceAtSquare h) t

initPieceAtSquare :: Square -> Piece
initPieceAtSquare (Square x y) 
  | y <= 3              = (Piece Starter North)
  | y >= boardSize - 2  = (Piece Starter South)
  | otherwise           = Empty


initPiece :: GameBoard -> Square -> Piece -> GameBoard
initPiece board sq piece =  Map.insert sq piece board



-- GAME DISPLAY (SIMPLE) ---------------

displayBoardHelper :: GameBoard -> Int -> Int -> String
displayBoardHelper board x y
    | y == boardSize+1  = ""
    | x == boardSize    = displayCell board boardSize y ++ "\n" ++ (displayBoardHelper board 1 (y+1))
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

printBoard = putStrLn (displayBoard startBoard)


-- GAME LOGIC

getActionsFromState :: InternalState -> [Action]
getActionsFromState state = getActionsFromSquares state squares

getActionsFromSquares :: InternalState -> [Square] -> [Action]
getActionsFromSquares state [] = []
getActionsFromSquares state (h:t) = getActionsFromSquare state h ++ (getActionsFromSquares state t)

getActionsFromSquare :: InternalState -> Square -> [Action]
getActionsFromSquare (GameState board playerType) sq = getActionsFromPiece (Map.lookup sq board) playerType sq

getActionsFromPiece :: Maybe Piece -> PlayerType -> Square -> [Action]
getActionsFromPiece Nothing _ _ = []
-- getActionsFromPiece (Piece pieceType piecePlayerType) playerType sq
--     | piecePlayerType /= playerType = []
--     | pieceType == King             = (jumpsNorth
-- TODO: Kings can move in any direction, other pieces can only move away from their player


getAllMovesFromSquare :: GameBoard -> Square -> [Square]
getAllMovesFromSquare board (Square x y) = [sq | sq <- allMoves, isSquareEmpty board sq]
    where
        allMoves = [(Square (x+1) (y+1)), (Square (x+1) (y-1)), (Square (x-1) (y+1)), (Square (x-1) (y-1))]


isSquareEmpty :: GameBoard -> Square -> Bool
isSquareEmpty board sq =
    case piece of
        Just Empty -> True
        _ -> False
    where
        piece = Map.lookup sq board




   
    
    
    
-- GAME CONTROLS & PERMISSIONS

data Action = Move Square Square
    | EndTurn


data Emote = Greet | Taunt | Lol | Nice

greetDefault = "Hi there!"
tauntDefault = "L2PLAY NOOB"
lolDefault = "HAHAHA"
niceDefault = "Nice Move.."

-- -- ??
-- listPlayerPieces :: GameBoard -> Player -> [Piece]
-- listPlayerPieces b p = []
--
-- -- ??
-- availableActions :: GameBoard -> Player -> [Move]
-- availableActions b p = []



-- FOR AI: probably rank available actions for whats best?
-- i.e. multijump > jump > just move
-- possibly get a second layer of what would AI's opponent do after, then 
-- possible moves after opponent's turn (i.e. AI's next turn), but thats starting 
-- to get into Hidden Markov Model type stuff

