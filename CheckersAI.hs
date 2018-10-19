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