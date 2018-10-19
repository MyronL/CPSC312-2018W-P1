module CheckersGraphicsBasicUserIO where


import CheckersData
import CheckersGraphicsBasicPrintLn
import Data.Map as Map
import Data.List as List
import System.IO
import Text.Read   (readMaybe)

import System.Exit
import Control.Concurrent



-- GAME DISPLAY (SIMPLE): SCREEN & USER DIALOGUE + INPUT ---------------

-- WELCOME SCREEN (pending)
welcomeScreen = ["\n",
    "********************************",
    "*                              *",    
    "* WELCOME TO HASKELL CHECKERS  *",
    "*                              *", 
    "********************************",
    "    Play at your own risk...    "]
-- 


startOptions = ["Commands:", 
    "Player VS Player (enter 1)", 
    "Player VS AI CPU (enter 2)", 
    "Exit (enter any other key)"]

exitPolitely = 
  do 
    putStrLn "\nExiting... Have a Nice Day =)"
    oneSecDelay
    exitWith ExitSuccess

displayPlayerTurn :: PlayerType -> Turn -> [Char]
displayPlayerTurn p t =  show p ++ " Player, "  ++ show t ++ ": " 



displayTurnMenu = "Controls: Move/Jump (enter), Concede (gg), Exit (xx)"

turnMenu :: [Move] -> IO Move
turnMenu moves =
  do
    putStrLn displayTurnMenu
    getTurnMenuResp moves

getTurnMenuResp moves =
  do
    response <- getLineCommand
    if (response == "gg")
        then
          checkConcede moves
    else if (response == "xx")
        then
          checkExit moves
    else
        getPieceMenuResp moves


displayPieceMenu = "Select your piece from below.\nEnter anything else to cancel"
getPieceMenuResp moves =
  do
    putStrLn displayPieceMenu
    let moveablePieces = getMoveablePieces moves
    putStrLn (showMoveablePieces moveablePieces)
    res <- getLineCommand
    let num = readMaybe res :: Maybe Int
    case num of
        Nothing -> turnMenu moves
        Just i ->
            if (i >= 1 && i <= length moveablePieces)
              then do
                putStrLn "OK"
                let sq = moveablePieces !! (i-1)
                getMoveJumpMenuResp moves sq
              else do
                turnMenu moves

showMoveablePieces :: [Square] -> String
showMoveablePieces squares = List.foldr (\ (i,sq) y -> "("++show i++")" ++ (sqToUI sq) ++ "  " ++ y) "" (zip [1..] squares)

getMoveablePieces :: [Move] -> [Square]
getMoveablePieces moves = nub [from | (Move from to) <- moves]

displayMoveJumpMenu = "Select your move from below.\nEnter anything else to cancel"
getMoveJumpMenuResp moves sq =
  do
    let validMoves = [(Move from to) | (Move from to) <- moves, from == sq]
    putStrLn displayMoveJumpMenu
    putStrLn (showMoves validMoves)
    res <- getLineCommand
    let num = readMaybe res :: Maybe Int
    case num of
        Nothing -> turnMenu moves
        Just i ->
            if (i >= 1 && i <= length validMoves)
              then do
                putStrLn "OK"
                let move = validMoves !! (i-1)
                return move
              else do
                turnMenu moves

showMoves :: [Move] -> String
showMoves moves = List.foldr (\ (i,(Move from to)) y -> "("++show i++")" ++ (sqToUI from) ++ "->" ++ (sqToUI to) ++ "  " ++ y) "" (zip [1..] moves)

checkConcede moves =
  do 
    putStrLn "Are you sure you want to Concede?"
    putStrLn "Type 'gg' to confirm. Otherwise return."
    res <- getLineCommand
    if (res == "gg")
      then do 
        -- TODO: trigger a loss here for current player (other player wins)
        putStrLn "Player Concedes..."
        return Concede
      else do
        turnMenu moves

checkExit moves =
  do 
    putStrLn "Are you sure you want to Exit?"
    putStrLn "Type 'xx' to confirm. Otherwise return."
    res <- getLineCommand
    if (res == "xx")
      then do 
        exitPolitely
      else do
        turnMenu moves


putStartOptions =  printLines startOptions

printLines [] = putStrLn ""
printLines (h:t) = 
    do
        putStrLn h
        printLines t

-- Runtime Delays 
oneSecDelay = threadDelay 1000000
oneHalfSecDelay = threadDelay 1500000
twoSecDelay = threadDelay 2000000
threeSecDelay = threadDelay 3000000


-- Adapter to Square coord: String -> Square x y
--strToSq (h:m:t) 
    

-- Adapter to Square coord: Square x y -> Dialogue String
sqToUI (Square x y) = "[" ++ [(xIntToStr x)] ++ show y ++ "]"


-- getLineCommand: Use this when expecting user-typed input (strings)
getLineCommand =
   do
     line <- getLine
     return (fixdel line)



humanPlayer :: IOPlayer
humanPlayer (State (GameState board playerType) moves) =
    do
        putStrLn ("Current player: " ++ (show playerType))
        putStrLn (displayBoard board)
        move <- turnMenu moves
        return move



{-
-- NOTE:

we NEED to borrow the fixdel code from Assignment3 
to allow backspace-deletes when using typed input
(just comes in handy when receiving in-game line commands)

-}
----- Two Implementations fo fixdel ----

-- fixdel removes deleted elements from string
fixdel st
   | '\DEL' `elem` st = fixdel (remdel st)
   | otherwise = st
remdel ('\DEL':r) = r
remdel (a:'\DEL':r) = r
remdel (a:r) = a: remdel r

-- ------------------------------------ --


