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



-- Given depth for pruning (should be even).
depth' :: Int 
depth' = 2

-- Given breadth for pruning.
breadth' :: Int
breadth' = 25 


-- Function that combines all the different parts implemented in Part I.
minimax' :: Player -> Game -> Action
minimax' playerMAX game = minimaxABFromTree (pruneBreadth breadth' (highFirst' (evalTree playerMAX (pruneDepth depth' (generateGameTree game) ))))

-- Given a game state, calls minimax and returns an action.
-- V: this receives the current board, where the minimax should play, and the list of players has the minimax as current player
minimaxAction' :: Board -> [Player] -> String -> Int -> Maybe Action
minimaxAction' b ps _ r = let g = Game b ps in minimaxAction'' g (minimax' (currentPlayer ps) g)
    where 
        -- Goes through the list of actions until it finds a valid one. 
        minimaxAction'' :: Game -> Action -> Maybe Action
        minimaxAction'' g' (Move s)
            | validStepAction g' s = Just (Move s)
            | otherwise = error "Minimax chose an invalid action."
        minimaxAction'' g' (Place w)
            | validWallAction g' w = Just (Place w)
            | otherwise = error "Minimax chose an invalid action."


wall1 :: Wall
wall1 = ((('d', 6), ('c', 7)), (('d', 6), ('d', 7)))

wall2 :: Wall
wall2 = ((('d', 6), ('f', 7)), (('g', 6), ('g', 7)))


reedPlayerAction :: Board -> [Player] -> String -> Int -> Maybe Action
reedPlayerAction b plrs str int
    | validWallAction (Game b plrs) (wall1) = Just (Place wall1)
    | validWallAction (Game b plrs) (wall2) = Just (Place wall2)
    | otherwise                             = minimaxAction' b plrs str int

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
