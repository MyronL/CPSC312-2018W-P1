module CheckersGame where


-- To run it, try:
-- ghci
-- :load CheckersGame
-- start

import CheckersData
import Data.Map as Map
import Data.List as List
import System.IO


-- This is the overall wrapper for the Checkers Game. 
-- This file should handle all turn-by-turn moves 
-- all the way to a winning player
-- *All UI Controls should be passed (and handled/verified) to here, from the actual players

start = runFirst

runFirst = 
    do
        printLines welcomeScreen
        putStartOptions
        response <- getLineCommand
        --checkMode response
        startGameTest        


startGame = 
  do
    printBoard
    putStrLn $ "PLAYER ONE: GO"
    response <- getLineCommand
    printBoard
    putStrLn $ "PLAYER TWO: GO"



-- a Turn has:
-- - turn number 
-- - player
-- - game state




-- (todo: pending)
welcomeScreen = ["WELCOME TO HASKELL CHECKERS",
    "Play at your own risk..."]
-- 
startOptions = ["Commands:", 
    "Player VS Player (enter 1)", 
    "Player VS AI CPU (enter 2)", 
    "Exit (enter any other key)"]


putStartOptions =  printLines startOptions

printLines [] = putStrLn ""
printLines (h:t) = 
    do
        putStrLn h
        printLines t


-- getLineCommand: Use this when expecting user-typed input (strings)
getLineCommand =
   do
     line <- getLine
     return (fixdel2 line)


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

-- (TEST)
-- (possibly how the game would look like)
-- 
startGameTest = 
   do
    printBoard
    putStrLn $ "PLAYER ONE TURN ONE: GO"
    response <- getLineCommand
    printBoard
    putStrLn $ "PLAYER TWO TURN ONE: GO"
    printBoard
    putStrLn $ "PLAYER ONE TURN TWO: GO"
    response <- getLineCommand
    printBoard
    putStrLn $ "PLAYER TWO TURN TWO: GO"
    response <- getLineCommand
    printBoard
    putStrLn $ "PLAYER ONE TURN THREE: GO"
    response <- getLineCommand
    printBoard
    putStrLn $ "PLAYER TWO TURN THREE: GO"
    response <- getLineCommand
    printBoard    
