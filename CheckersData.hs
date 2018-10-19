module CheckersData where

import Data.Map as Map
import Data.List as List

-- a Game takes a move and the current state and returns a result
type Game = Move -> State -> Result

-- a Move is a move from one square to another or conceding the game
data Move = Move Square Square
    | Concede
    deriving (Show, Eq)

-- the State keeps track of the internal state of the game and the available moves
data State = State InternalState [Move]

-- a Result is what happens after each move
data Result = EndOfGame PlayerType GameBoard    -- end of game, value, ending state
            | MyTurn State              -- continue current player's turn with new state
            | YourTurn State            -- continue next player's turn with new state
            | InvalidMove State

-- Current Game State:
-- - Gameboard
-- - Player's turn
data InternalState = GameState GameBoard PlayerType

-- a Player takes the current state of the game and returns a move
type Player = State -> Move
type IOPlayer = State -> IO Move

-- PlayerType is the two players in the game
data PlayerType = North | South
  deriving (Eq, Show)

-- flipPlayer returns the opposite of the given player type
flipPlayer :: PlayerType -> PlayerType
flipPlayer North = South
flipPlayer South = North

-- a Piece is a checkers piece, or empty
data Piece = Piece PieceType PlayerType
    | Empty
  deriving (Eq, Show)

data PieceType = Starter | King
  deriving (Eq, Show)



-- Square is a single square tile on a checkers grid
data Square = Square Int Int
    deriving (Show)

instance Eq Square where
    (Square a b) == (Square c d) = a == c && b == d

instance Ord Square where
    (Square a b) <= (Square c d) = a < c || (a == c && b <= d)


-- GameBoard is a map from a square to the piece in that square
type GameBoard = Map Square Piece




-- GAME INIT ---------------------------

-- boardSize is the number of squares along the edge of the board
boardSize = 6

-- the number of checker rows for each player
numRows = min 3 (div (boardSize-1) 2)

-- All Steppable Checkers Squares: 
--    (1,1), (2,1) ...
--    (1,2), (2,2) ...
squares = [(Square x y) | y <- [1..boardSize], x <- [1..boardSize], (mod x 2 == 0 && mod y 2 == 1) || (mod x 2 == 1 && mod y 2 == 0)]


-- the initial game board
startBoard = initSquares Map.empty squares

-- initSquares takes a board and a list of squares and initializes each square in the board
initSquares :: GameBoard -> [Square] -> GameBoard
initSquares board [] = board
initSquares board (h:t) = initSquares (initPiece board h $ initPieceAtSquare h) t

-- initPiece adds a piece to the game board at a square
initPiece :: GameBoard -> Square -> Piece -> GameBoard
initPiece board sq piece =  Map.insert sq piece board

-- initPieceAtSquare returns a piece to be put into the given square
initPieceAtSquare :: Square -> Piece
initPieceAtSquare (Square x y) 
  | y <= numRows            = (Piece Starter North)
  | y > boardSize - numRows = (Piece Starter South)
  | otherwise               = Empty





-- GAME LOGIC ---------------------------

-- getActionsFromState returns all possible moves in the given state
getActionsFromState :: InternalState -> [Move]
getActionsFromState state =
    if jumpMoves == []
    then getActionsFromSquares state squares getAllMovesFromSquare
    else jumpMoves
    where
        jumpMoves = getActionsFromSquares state squares getAllJumpsFromSquare

-- getActionsFromSquares returns all possible moves at all squares for either jumps or regular moves
getActionsFromSquares :: InternalState -> [Square] -> MoveGetter -> [Move]
getActionsFromSquares _ [] _ = []
getActionsFromSquares state (h:t) moveGetter = getActionsFromSquare state h moveGetter ++ (getActionsFromSquares state t moveGetter)

-- getActionsFromSquare returns all possible moves from a given square for either jumps or regular moves
getActionsFromSquare :: InternalState -> Square -> MoveGetter -> [Move]
getActionsFromSquare (GameState board playerType) sq moveGetter =
    case Map.lookup sq board of
        Just (Piece pieceType piecePlayerType) -> getActionsFromPiece board playerType sq pieceType piecePlayerType moveGetter
        _ -> []

-- getActionsFromPiece returns all possible moves for a certain piece
getActionsFromPiece :: GameBoard -> PlayerType -> Square -> PieceType -> PlayerType -> MoveGetter -> [Move]
getActionsFromPiece board playerType sq pieceType piecePlayerType moveGetter
     | piecePlayerType /= playerType = []
     | pieceType == King             = moves
     | otherwise                     = getForwardMoves moves playerType
     where moves = moveGetter board sq

-- getForwardMoves filters a list of moves so that they are only going forward relative to a player
getForwardMoves :: [Move] -> PlayerType -> [Move]
getForwardMoves moves playerType = [move | move <- moves, comparator move]
    where
        comparator = case playerType of
            North -> goingSouth
            South -> goingNorth

-- goingNorth and goingSouth indicate which direction the move is going
goingNorth :: Move -> Bool
goingNorth move = 0 > verticalMovement move
goingSouth :: Move -> Bool
goingSouth move = 0 < verticalMovement move

-- verticalMovement returns the number of squares the move is going up or down
verticalMovement :: Move -> Int
verticalMovement (Move (Square _ y1) (Square _ y2)) = y2 - y1

-- A move getter is a function that gets all moves of a certain type from a square
type MoveGetter = GameBoard -> Square -> [Move]


-- getAllJumpsFromSquare returns all possible jumps from a square
getAllJumpsFromSquare :: MoveGetter
getAllJumpsFromSquare board (Square x y) = [Move (Square x y) sq | sq <- allMoves, isSquareEmpty board sq, isJumpLegal board (Move (Square x y) sq)]
    where
        allMoves = [(Square (x+2) (y+2)), (Square (x+2) (y-2)), (Square (x-2) (y+2)), (Square (x-2) (y-2))]

-- isJumpLegal makes sure that a jump is going over an opponent's piece
isJumpLegal :: GameBoard -> Move -> Bool
isJumpLegal board (Move from to) =
    case Map.lookup from board of
        Just (Piece _ playerType) ->
            case getJumpedPiece board (Move from to) of
                Just (Piece _ jumpedPlayer) -> playerType /= jumpedPlayer
                _ -> False
        _ -> False

-- getJumpedPiece returns the piece that was jumped over
getJumpedPiece :: GameBoard -> Move -> Maybe Piece
getJumpedPiece board move = Map.lookup (getJumpedSquare move) board

-- getJumpedSquare returns the square that was jumped over
getJumpedSquare :: Move -> Square
getJumpedSquare (Move (Square fromX fromY) (Square toX toY)) = Square (div (fromX + toX) 2) (div (fromY + toY) 2)


-- getAllMovesFromSquare returns all possible regular moves from a square
getAllMovesFromSquare :: MoveGetter
getAllMovesFromSquare board (Square x y) = [Move (Square x y) sq | sq <- allMoves, isSquareEmpty board sq]
    where
        allMoves = [(Square (x+1) (y+1)), (Square (x+1) (y-1)), (Square (x-1) (y+1)), (Square (x-1) (y-1))]


-- isSquareEmpty indicates if a square has no piece
isSquareEmpty :: GameBoard -> Square -> Bool
isSquareEmpty board sq =
    case Map.lookup sq board of
        Just Empty -> True
        _ -> False



-- getState returns the State with all available moves from the internal state
getState :: InternalState -> State
getState internalState = State internalState (getActionsFromState internalState)

-- startState is the initial state
startState = getState (GameState startBoard South)



-- GAME RESULTS ---------------------------

-- checkers is the Game implementation
checkers :: Game
checkers move (State internalState availableMoves)
    | elem move availableMoves = if isJump move then getResultFromJump move internalState else getResultFromMove move internalState
    | move == Concede = concede internalState
    | otherwise                 = InvalidMove (State internalState availableMoves)

-- concede ends the game in favour of the opponent
concede :: InternalState -> Result
concede (GameState board playerType) = EndOfGame (flipPlayer playerType) board


-- isJump returns whether or not a move is a jump
isJump :: Move -> Bool
isJump move = 2 == abs (verticalMovement move)


-- getResultFromMove returns the game result from a move
getResultFromMove :: Move -> InternalState -> Result
getResultFromMove (Move from to) (GameState board playerType)
    | isWin newBoard playerType = EndOfGame playerType newBoard
    | otherwise               = YourTurn (getState (GameState newBoard (flipPlayer playerType)))
    where
        newBoard = Map.insert from Empty (Map.insert to newPiece board)
        newPiece = getNewPiece (board ! from ) to

-- getResultFromMove returns the game result from a jump
getResultFromJump :: Move -> InternalState -> Result
getResultFromJump (Move from to) (GameState board playerType)
    | isWin newBoard playerType = EndOfGame playerType newBoard
    | newJumpMoves /= []        = MyTurn (State (GameState newBoard playerType) newJumpMoves)
    | otherwise                 = YourTurn (getState (GameState newBoard (flipPlayer playerType)))
    where
        boardAfterJump = Map.insert from Empty (Map.insert to newPiece board)
        newBoard = Map.insert (getJumpedSquare (Move from to)) Empty boardAfterJump
        newJumpMoves = getActionsFromSquare (GameState newBoard playerType) to getAllJumpsFromSquare
        newPiece = getNewPiece (board ! from ) to


-- isWin checks if the opponent has no possible moves
isWin :: GameBoard -> PlayerType -> Bool
isWin board playerType
    | getActionsFromState (GameState board (flipPlayer playerType)) == [] = True
    | otherwise = False

-- getNewPiece returns the piece after it moves
getNewPiece :: Piece -> Square -> Piece
getNewPiece (Piece pieceType playerType) (Square x y)
    | y == 1 || y == boardSize  = (Piece King playerType)
    | otherwise                 = (Piece pieceType playerType)