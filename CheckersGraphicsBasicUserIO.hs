module CheckersGraphicsBasicUserIO where


import CheckersData
import CheckersGraphicsBasicPrintLn
import Data.Map as Map
import Data.List as List
import System.IO

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



displayTurnMenu = "Controls: Move/Jump (enter 1), Concede (gg), Exit (xx)"
-- TODO: sample
displayMoveJumpCtrls = "(0)[b2] (0)[b2] (0)[b2] (0)[b2] (0)[b2] (0)[b2] (0)[b2] (0)[b2] (0)[b2] (0)[b2] (0)[b2] (0)[b2] "

turnMenu = 
  do
    putStrLn displayTurnMenu
    getTurnMenuResp

getTurnMenuResp = 
  do
    response <- getLineCommand
    if (response == "gg")
        then
          checkConcede
    else if (response == "xx")
        then
          checkExit
    else
        return (readMove response)

-- TODO: read move somehow
readMove :: String -> Maybe Move
readMove response = Just (Move (Square 1 2) (Square 3 4))


-- -- TODO: (count should be actual player's piece count)
-- displayPieceMenu = "Select your pieces from numbers 1 to (count) below. \n Enter anything else to cancel"
-- getPieceMenuResp =
--   do
--     putStrLn displayPieceMenu
--     -- TODO: replace line below with list of player's pieces, as squares, in the same format as below
--     putStrLn displayMoveJumpCtrls
--     res <- getLineCommand
--     -- TODO: validations to pick the right square piece, then
--     if (res == " ")
--       then do
--         putStrLn "OK"
--         getMoveJumpMenuResp
--       else do
--         turnMenu
--
-- -- TODO: sample
-- displayMoveJumps = "(0)[b2]->[c3] (0)[b2]->[a3]"
-- displayMoveJumpMenu = "Select your moves from numbers 1 to (count) below. \n Enter anything else to cancel"
-- getMoveJumpMenuResp =
--   do
--     putStrLn displayMoveJumpMenu
--     putStrLn displayMoveJumps
--     res <- getLineCommand
--     -- TODO: validations to pick the right square piece, then
--     if (res == " ")
--       then do
--         putStrLn "OK"
--       else do
--         getPieceMenuResp

checkConcede = 
  do 
    putStrLn "Are you sure you want to Concede?"
    putStrLn "Type 'gg' to confirm. Otherwise return."
    res <- getLineCommand
    if (res == "gg")
      then do 
        -- TODO: trigger a loss here for current player (other player wins)
        putStrLn "Player Concedes..."
        return Nothing
      else do
        turnMenu

checkExit = 
  do 
    putStrLn "Are you sure you want to Exit?"
    putStrLn "Type 'xx' to confirm. Otherwise return."
    res <- getLineCommand
    if (res == "xx")
      then do 
        exitPolitely
      else do
        turnMenu


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


-- Adapters for x-coord: (a..h) -> (1..8), and vice versa
-- (for passing to the Data Model layer)
xStrToInt i
  | i == "a" = 1
  | i == "b" = 2 
  | i == "c" = 3
  | i == "d" = 4
  | i == "e" = 5
  | i == "f" = 6    
  | i == "g" = 7  
  | i == "h" = 8
  | otherwise = 0   -- invalid if coord = 0


xIntToStr i 
  | i == 1 = "a"
  | i == 2 = "b"  
  | i == 3 = "c"
  | i == 4 = "d"
  | i == 5 = "e"
  | i == 6 = "f"    
  | i == 7 = "g"  
  | i == 8 = "h"
  | otherwise = "?"  -- invalid if coord = ?


-- Adapter to Square coord: String -> Square x y
--strToSq (h:m:t) 
    

-- Adapter to Square coord: Square x y -> Dialogue String
sqToUI (Square x y) = "[" ++ (xIntToStr x) ++ show y ++ "]"


-- getLineCommand: Use this when expecting user-typed input (strings)
getLineCommand =
   do
     line <- getLine
     return (fixdel2 line)



humanPlayer :: IOPlayer
humanPlayer (State (GameState board playerType) (h:t)) =
    do
        response <- turnMenu
        return h
        -- TODO: uncomment this when move reading is complete
--         case response of
--             Just move -> return move
--             _ ->
--                 do
--                     putStrLn "Couldn't read move, please try again"
--                     humanPlayer (State (GameState board playerType) (h:t))



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

-- fixdel2 deleted elements from string 
fixdel2 :: [Char] -> [Char]
fixdel2 st = fst (remdel2 st)
-- remdel2 st   returns (resulting_string, number_of_deletes_to_do)
remdel2 :: [Char] -> ([Char], Int)
remdel2 [] = ([],0)
remdel2 ('\DEL':t) = (s,n+1)
    where (s,n) = remdel2 t
remdel2 (h:t)
    | n>0 = (s,n-1)
    | otherwise = (h:s,0)
    where (s,n) = remdel2 t

-- ------------------------------------ --


