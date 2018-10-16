
import Data.Map as Map
import Data.List as List



data Player = North
  | South
  deriving (Eq)  

data PieceType = Starter
  | King
  deriving (Eq)
  
data Piece = Piece PieceType Player
  | Empty
  deriving (Eq)



-- SINGLE SQUARE TILE IN A CHECKERS BOARD
data Square = Square Int Int

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



data GameBoard = GameBoard (Map Square Piece) 



-- Current Game State:
-- - Gameboard
-- - Player's turn
-- - List of legal moves
-- - Player's action
data GameState = GameState GameBoard Player [Move] Action


-- GAME INIT ---------------------------



-- All Steppable Checkers Squares: 
--    (2,1), (4,1) ... 
--    (1,2), (3,2) ...
squares = [(Square x y) | x <- [1..8], y <- [1..8], (mod x 2 == 0 && mod y 2 == 1) || (mod x 2 == 1 && mod y 2 == 0)]


startGame = initSquares (GameBoard (Map.empty)) squares



initSquares :: GameBoard -> [Square] -> GameBoard 
initSquares (GameBoard m) [] = (GameBoard m)
initSquares (GameBoard m) (h:t) = initSquares (initPiece (GameBoard m) h $ initPieceAtSquare h) t

initPieceAtSquare :: Square -> Piece
initPieceAtSquare (Square x y) 
  | y <= 3 = (Piece Starter North)
  | y >= 6 = (Piece Starter South)
  | otherwise = Empty


initPiece :: GameBoard -> Square -> Piece -> GameBoard
initPiece (GameBoard b) sq piece = GameBoard $ Map.insert sq piece b



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