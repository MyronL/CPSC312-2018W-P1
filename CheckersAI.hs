module CheckersAI where

import CheckersData
import CheckersGraphicsBasicUserIO

-- simpleComputerPlayer is a computer that takes the first move it sees
simpleComputerPlayer :: Player
simpleComputerPlayer (State (GameState board playerType) (h:t)) = h
simpleComputer :: IOPlayer
simpleComputer = createIOPlayer simpleComputerPlayer

-- createIOPlayer creates a player that can use IO from a regular player
createIOPlayer :: Player -> IOPlayer
createIOPlayer player state =
    do
        let (Move from to) = player state
        putStrLn ("Computer moved " ++ sqToUI from ++ "->" ++ sqToUI to)
        oneSecDelay --this is so user recognizes the AI move
        return (Move from to)


-- minimaxIO is the IOPlayer that runs the minimax ai
minimaxIO :: Game -> IOPlayer
minimaxIO game state =
    do
        let ((Move from to), score) = minimax game state 6 -- this is the maximum depth of the search
        putStrLn ("Computer moved " ++ sqToUI from ++ "->" ++ sqToUI to ++ " with score " ++ show score)
        oneSecDelay --this is so user recognizes the AI move
        return (Move from to)

-- We are using a modified version of the minimax functionality from the lectures
-- minimax returns the best move and its score for a current state in a game
minimax:: Game -> State -> Int -> (Move, Double)
minimax game st depth =
      argmax (valueact game st depth) avail
      where State _ avail = st

-- valueact game st action  is the value of doing action act in state st for game
valueact :: Game -> State -> Int -> Move -> Double
valueact game st depth act = value game (game act st) depth

-- value game result  = value  for current player after result
value:: Game -> Result -> Int -> Double
value _  (EndOfGame player _) _ = 1 -- the current player won
value _ result 0 = getScoreOfResult result -- when it reaches the maximum depth, calculate a score based on the current state
value game (YourTurn st) depth =  - snd (minimax game st (depth-1))
value game (MyTurn st) depth =  snd (minimax game st (depth-1))


-- argmax f lst  = (e, f e) for e <- lsts where f e is maximal
argmax :: Ord v => (e -> v) -> [e] -> (e,v)
argmax f [e] = (e, f e)
argmax f (h:t)
   | fh > ft = (h,fh)
   | otherwise = (bt, ft)
   where
      (bt,ft) = argmax f t
      fh = f h

-- getScoreOfResult and getScoreOfBoard gets the score for the current state based on the number of pieces each player has
getScoreOfResult :: Result -> Double
getScoreOfResult (YourTurn (State internalState _)) = getScoreOfBoard internalState
getScoreOfResult (MyTurn (State internalState _)) = getScoreOfBoard internalState

getScoreOfBoard :: InternalState -> Double
getScoreOfBoard (GameState board playerType) = 2*(northPieces / (northPieces + southPieces)) - 1
    where
        (northPieces, southPieces) = foldr (\ piece (np, sp) ->
                case piece of
                    (Piece _ player) ->
                        if player /= playerType
                        then (np+1, sp)
                        else (np, sp+1)
                    _ -> (np, sp)
            ) (0,0) board