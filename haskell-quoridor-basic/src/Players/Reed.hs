{-
    Module: Reed.

    *** PART III (10 pt) ***

    Define a player that uses teh Reed opening and play against it. Is the Reed opening a good 
    opening? Write your answers in Reed.txt.
-}
module Players.Reed where

import Types
import Action
import Game
import Players.Minimax

-- Create a player that starts with the Reed opening. After that, you may use your minimax action or
-- the given action for DumbPlayer. 
-- [Hint 1: Use the variable 'turn' in Player.]
-- [Hint 2: Use 'wallTop' to get the walls you need.]
-- [Hint 3: Don't forget to check that the action is valid using 'validWallAction'.]

wall1 :: Wall
wall1 = ((('c', 3), ('c', 4)), (('d', 3), ('d', 4)))

wall2 :: Wall
wall2 = ((('f', 3), ('f', 4)), (('g', 3), ('g', 4)))


reedPlayerAction :: Board -> [Player] -> String -> Int -> Maybe Action
reedPlayerAction b plrs str int
    | validWallAction (Game b plrs) (wall1) = Just (Place wall1)
    | validWallAction (Game b plrs) (wall2) = Just (Place wall2)
    | otherwise                      = minimaxAction b plrs str int

-- We build a Reed player from a name, a starting cell, a number of walls, an array of winning
-- positions and 'commandToAction'.
makeReedPlayer :: String -> Cell -> Int -> [Cell] -> Player
makeReedPlayer n c rws wps = Player {
    name = n,
    turn = 1,
    currentCell = c, 
    remainingWalls = rws,
    winningPositions = wps,
    isHuman = False,
    chooseAction = reedPlayerAction } 
