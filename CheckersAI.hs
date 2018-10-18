module CheckersAI where

import CheckersData


simpleComputer :: Player
simpleComputer (State (GameState board playerType) (h:t)) = h

computer = createIOPlayer simpleComputer