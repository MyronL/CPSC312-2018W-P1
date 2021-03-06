module CheckersGame where


-- To run it, try:
-- ghci
-- :load CheckersGame
-- start

import CheckersData
import CheckersGraphicsBasicPrintLn
import CheckersGraphicsBasicUserIO
import CheckersAI
import Data.Map as Map
import Data.List as List
import System.IO
import System.Exit
import Control.Concurrent



-- This is the overall wrapper for the Checkers Game. 
-- This file should handle all turn-by-turn moves 
-- all the way to a winning player
-- *All UI Controls should be passed (and handled/verified) to here, from the actual players

-- start is the way to start the program
start = runFirst

-- runFirst prints out the text at the beginning of the program
runFirst = 
    do
        printLines welcomeScreen
        putStartOptions
        getStartOptions

-- getStartOptions gets the options for when you first open the program
getStartOptions =
  do
    response <- getChar
    if (response == '1')
       then do
          putStrLn "\nPlayer VS Player Selected. "
          putStrLn "\nReady..."
          oneHalfSecDelay
          putStrLn "\nSTART! "
          play checkers (YourTurn startState) humanPlayer humanPlayer
       else if (response == '2')
       then do
          putStrLn "\nPlayer VS AI CPU Selected."
          putStrLn "\nReady..."
          oneHalfSecDelay
          putStrLn "\nSTART! "
          play checkers (YourTurn startState) humanPlayer (minimaxIO checkers)
       else do
          exitPolitely


-- play plays a game between two players until someone wins
play :: Game -> Result -> IOPlayer -> IOPlayer -> IO ()
play game (YourTurn (State (GameState board playerType) moves)) player1 player2 =
    do
        putStrLn "======================================================================="
        action <- player1 (State (GameState board playerType) moves)
        let newResult = game action (State (GameState board playerType) moves)
        play game newResult player2 player1

play game (MyTurn (State (GameState board playerType) moves)) player1 player2 =
    do
        putStrLn "======================================================================="
        action <- player2 (State (GameState board playerType) moves)
        let newResult = game action (State (GameState board playerType) moves)
        play game newResult player1 player2

play game (EndOfGame winningPlayer board) player1 player2 =
    do
        putStrLn "======================================================================="
        putStrLn (displayBoard board)
        putStrLn ("The winner is " ++ (show winningPlayer))

play game (InvalidMove state) player1 player2 =
    do
        putStrLn "That is not a valid move, please try again"
        play game (YourTurn state) player2 player1
