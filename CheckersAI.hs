module CheckersAI where

import CheckersData
import CheckersGraphicsBasicUserIO


simpleComputerPlayer :: Player
simpleComputerPlayer (State (GameState board playerType) (h:t)) = h

simpleComputer :: IOPlayer
simpleComputer = createIOPlayer simpleComputerPlayer


createIOPlayer :: Player -> IOPlayer
createIOPlayer player state =
    do
        let (Move from to) = player state
        putStrLn ("Computer moved " ++ sqToUI from ++ "->" ++ sqToUI to)
        oneSecDelay --this is so user recognizes the AI move
        return (Move from to)


aiPlayer = North


minimaxIO :: Game -> IOPlayer
minimaxIO game state =
    do
        let ((Move from to), score) = minimax game state 6
        putStrLn ("Computer moved " ++ sqToUI from ++ "->" ++ sqToUI to ++ " with score " ++ show score)
        oneSecDelay --this is so user recognizes the AI move
        return (Move from to)


----   Determining the best move  ---
minimax:: Game -> State -> Int -> (Move, Double)
-- minimax game state   =>  (move,value_to_player)
-- precondition: there are some moves that are available
minimax game st depth =
      argmax (valueact game st depth) avail
      where State _ avail = st

-- valueact game st action  is the value of doing action act in state st for game
valueact :: Game -> State -> Int -> Move -> Double
valueact game st depth act = value game (game act st) depth

-- value game result  = value  for current player after result
value:: Game -> Result -> Int -> Double
value _  (EndOfGame player _) _ = 1
value _ result 0 = getScoreOfResult result
value game (YourTurn st) depth =  - snd (minimax game st (depth-1))
value game (MyTurn st) depth =  snd (minimax game st (depth-1))


mm_player:: Game -> Player
mm_player game state = fst ( minimax game state 6)


-- argmax f lst  = (e, f e) for e <- lsts where f e is maximal
--  Note that this does not require the elements of lst to be comparable
-- like  max[(f e,e) <- e in lst] but where only the first elements of pairs are compared in the max.
argmax :: Ord v => (e -> v) -> [e] -> (e,v)
argmax f [e] = (e, f e)
argmax f (h:t)
   | fh > ft = (h,fh)
   | otherwise = (bt, ft)
   where
      (bt,ft) = argmax f t
      fh = f h


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