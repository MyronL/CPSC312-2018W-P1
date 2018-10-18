module CheckersGame where


-- To run it, try:
-- ghci
-- :load CheckersGame
-- start

import CheckersData
import CheckersGraphicsBasicPrintLn
import CheckersGraphicsBasicUserIO
import Data.Map as Map
import Data.List as List
import System.IO
import System.Exit
import Control.Concurrent



-- This is the overall wrapper for the Checkers Game. 
-- This file should handle all turn-by-turn moves 
-- all the way to a winning player
-- *All UI Controls should be passed (and handled/verified) to here, from the actual players

start = runFirst

runFirst = 
    do
        printLines welcomeScreen

        putStartOptions
        getStartOptions
        startGameTest


startGame = 
  do
    printBoard
    putStrLn $ "PLAYER ONE: GO"
    response <- getLineCommand
    putStrLn $ response
    printBoard
    putStrLn $ "PLAYER TWO: GO"



-- a Turn has:
-- - turn number 
-- - player
-- - game state


-- (TEST)
-- (possibly how the game would look like)
-- 
startGameTest = 
   do
    printBoard
    --putStrLn $ "PLAYER ONE TURN ONE: GO"
    putStrLn $ displayPlayerTurn North (Turn 1) ++ "GO!" 
    turnMenu
    printBoard
    putStrLn $ displayPlayerTurn South (Turn 1) ++ "GO!" 
    turnMenu
    printBoard
    putStrLn $ displayPlayerTurn North (Turn 2) ++ "GO!" 
    turnMenu
    printBoard
    putStrLn $ displayPlayerTurn South (Turn 2) ++ "GO!" 
    turnMenu
    printBoard
    putStrLn $ displayPlayerTurn North (Turn 3) ++ "GO!" 
    turnMenu
    printBoard
    putStrLn $ displayPlayerTurn South (Turn 3) ++ "GO!" 
    turnMenu
    printBoard



play game (YourTurn (State (GameState board playerType) moves)) player1 player2 =
    do
        putStrLn (displayBoard board)
        action <- player1 (State (GameState board playerType) moves)
        let newResult = game action (State (GameState board playerType) moves)
        play game newResult player2 player1

play game (MyTurn (State (GameState board playerType) moves)) player1 player2 =
    do
        putStrLn (displayBoard board)
        action <- player2 (State (GameState board playerType) moves)
        let newResult = game action (State (GameState board playerType) moves)
        play game newResult player1 player2

play game (EndOfGame winningPlayer board) player1 player2 =
    do
        putStrLn (displayBoard board)
        putStrLn ("The winner is " ++ (show winningPlayer))


playCheckers = play checkers (YourTurn startState) humanPlayer humanPlayer

humanPlayer :: State -> IO Move
humanPlayer (State (GameState board playerType) (h:t)) =
    do
        putStrLn "Enter your move"
        line <- getLine
        return h


simpleComputer :: State -> IO Move
simpleComputer (State (GameState board playerType) (h:t)) =
    do
        return h
