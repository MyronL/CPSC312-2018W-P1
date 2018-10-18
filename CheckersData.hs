module CheckersData where

import Data.Map as Map
import Data.List as List
import System.IO

-- To run it, try:
-- ghci
-- :load TwentyQs
-- go

data Turn = Turn Int
  deriving (Eq, Ord, Show)

data PlayerType = North | South
  deriving (Eq, Show)

flipPlayer :: PlayerType -> PlayerType
flipPlayer North = South
flipPlayer South = North

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

data State = State InternalState [Move]  -- internal_state available_actions

data Result = EndOfGame Double State    -- end of game, value, starting state
            | MyTurn State              -- continue current player's turn with new state
            | YourTurn State            -- continue next player's turn with new state
            | InvalidMove

type Game = Move -> State -> Result

type Player = State -> Move


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



-- GAME LOGIC

getActionsFromState :: InternalState -> [Move]
getActionsFromState state =
    if jumpMoves == []
    then getActionsFromSquares state squares getAllMovesFromSquare
    else jumpMoves
    where
        jumpMoves = getActionsFromSquares state squares getAllJumpsFromSquare

getActionsFromSquares :: InternalState -> [Square] -> MoveGetter -> [Move]
getActionsFromSquares _ [] _ = []
getActionsFromSquares state (h:t) moveGetter = getActionsFromSquare state h moveGetter ++ (getActionsFromSquares state t moveGetter)

getActionsFromSquare :: InternalState -> Square -> MoveGetter -> [Move]
getActionsFromSquare (GameState board playerType) sq moveGetter =
    case Map.lookup sq board of
        Just (Piece pieceType piecePlayerType) -> getActionsFromPiece board playerType sq pieceType piecePlayerType moveGetter
        _ -> []

getActionsFromPiece :: GameBoard -> PlayerType -> Square -> PieceType -> PlayerType -> MoveGetter -> [Move]
getActionsFromPiece board playerType sq pieceType piecePlayerType moveGetter
     | piecePlayerType /= playerType = []
     | pieceType == King             = moves
     | otherwise                     = getForwardMoves moves playerType
     where moves = moveGetter board sq

getForwardMoves :: [Move] -> PlayerType -> [Move]
getForwardMoves moves playerType = [move | move <- moves, comparator move]
    where
        comparator = case playerType of
            North -> goingSouth
            South -> goingNorth

verticalMovement :: Move -> Int
verticalMovement (Move (Square _ y1) (Square _ y2)) = y2 - y1

goingNorth :: Move -> Bool
goingNorth move = 0 > verticalMovement move

goingSouth :: Move -> Bool
goingSouth move = 0 < verticalMovement move

isJumpLegal :: GameBoard -> Move -> Bool
isJumpLegal board (Move from to) =
    case Map.lookup from board of
        Just (Piece _ playerType) ->
            case getJumpedPiece board (Move from to) of
                Just (Piece _ jumpedPlayer) -> playerType /= jumpedPlayer
                _ -> False
        _ -> False

getJumpedPiece :: GameBoard -> Move -> Maybe Piece
getJumpedPiece board move = Map.lookup (getJumpedSquare move) board

getJumpedSquare :: Move -> Square
getJumpedSquare (Move (Square fromX fromY) (Square toX toY)) = Square (div (fromX + toX) 2) (div (fromY + toY) 2)


type MoveGetter = GameBoard -> Square -> [Move]

getAllJumpsFromSquare :: MoveGetter
getAllJumpsFromSquare board (Square x y) = [Move (Square x y) sq | sq <- allMoves, isSquareEmpty board sq, isJumpLegal board (Move (Square x y) sq)]
    where
        allMoves = [(Square (x+2) (y+2)), (Square (x+2) (y-2)), (Square (x-2) (y+2)), (Square (x-2) (y-2))]


getAllMovesFromSquare :: MoveGetter
getAllMovesFromSquare board (Square x y) = [Move (Square x y) sq | sq <- allMoves, isSquareEmpty board sq]
    where
        allMoves = [(Square (x+1) (y+1)), (Square (x+1) (y-1)), (Square (x-1) (y+1)), (Square (x-1) (y-1))]


isSquareEmpty :: GameBoard -> Square -> Bool
isSquareEmpty board sq =
    case Map.lookup sq board of
        Just Empty -> True
        _ -> False



getState :: InternalState -> State
getState internalState = State internalState (getActionsFromState internalState)

startState = getState (GameState startBoard South)

checkers :: Game
checkers move (State internalState availableMoves)
    | elem move availableMoves = if isJump move then getResultFromJump move internalState else getResultFromMove move internalState
    | otherwise                 = InvalidMove


isJump :: Move -> Bool
isJump move = 2 == abs (verticalMovement move)


getResultFromMove :: Move -> InternalState -> Result
getResultFromMove (Move from to) (GameState board playerType)
    | isWin newBoard playerType = EndOfGame 1 startState
    | otherwise               = YourTurn (getState (GameState newBoard (flipPlayer playerType)))
    where newBoard = Map.insert from Empty (Map.insert to (board ! from ) board)

-- TODO: fix this
getResultFromJump :: Move -> InternalState -> Result
getResultFromJump (Move from to) (GameState board playerType)
    | isWin newBoard playerType = EndOfGame 1 startState
    | otherwise               = YourTurn (getState (GameState newBoard (flipPlayer playerType)))
    where newBoard = Map.insert from Empty (Map.insert to (board ! from ) board)


isWin :: GameBoard -> PlayerType -> Bool
isWin board playerType = False
    
-- GAME CONTROLS & PERMISSIONS
data Move = Move Square Square
    deriving (Show, Eq)


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

