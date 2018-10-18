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



-- (TEST)
startGame = 
  do
    printBoard
    putStrLn $ "PLAYER ONE: GO"
