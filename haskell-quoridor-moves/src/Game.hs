{-
    Module: Game.

    Functions used in the game loop to change the game state.
-}
module Game where

import Types
import Constants
import Action
import Board
import Player




{-
    'performAction' and helpers.
-}

-- The current player is the first element in the players list.
currentPlayer :: [Player] -> Player 
currentPlayer = head

-- The previous player is the last element in the players list.
previousPlayer :: [Player] -> Player 
previousPlayer = last

-- The player that just played goes to the back of the list. Used to change turn.
rotatePlayers :: [Player] -> [Player]
rotatePlayers [] = [] 
rotatePlayers (p:ps) = ps ++ [p]

-- A step action is valid if the step is valid and no other player is in the target cell (canMove).
validStepAction :: Game -> Step -> Bool
validStepAction (Game b ps) s = (canMove (currentPlayer ps) ps s) && (validStep b s)

-- Generate all valid steps at a game state.
validSteps :: Game -> [Action]
validSteps g@(Game b ps) = map Move (filter (validStepAction g) steps)
    where  
        steps = let p = currentPlayer ps in makeSteps (currentCell p) (adjacentCells p)

-- ! helper methods fod validWallAction
removeDuplicates' :: (Eq a) => [a] -> [a]
removeDuplicates' items = helper [] items
    where helper seen [] = seen
          helper seen (x:xs)
              | x `elem` seen = helper seen xs
              | otherwise = helper (seen ++ [x]) xs
-- * returns the shortest distance from the current cell to the winning cells,
distWinning' :: Board -> [Cell] -> [Cell] -> [Cell] -> Int
distWinning' bd [] wc visC = 999  -- this means that there is no path to the winning cells
distWinning' bd cTE wc visC
    | helper cTE wc    = 0
    | otherwise        = 1 + distWinning' bd (removeDuplicates' reachableC) wc (visC ++ (removeDuplicates' reachableC))

        where
            helper [] b = False
            helper (x:xs) b = (x `elem` b) || helper xs b

            reachableC = foldr (++) [] [[ newCell | newCell <- reachableCells bd prevCell, not (newCell `elem` visC)] | prevCell <- cTE]

-- A wall action is valid if the wall is valid and player has walls remaining.
-- ! this was edited to make sure that no walls can be placed to block the opponent
validWallAction :: Game -> Wall -> Bool 
validWallAction (Game b ps) w = (hasWallsLeft (currentPlayer ps)) && (validWall b w) && ((distWinning' (placeWall b w) [currentCell (currentPlayer ps)] (winningPositions (currentPlayer ps)) [currentCell (currentPlayer ps)]) < 999) && ((distWinning' (placeWall b w) [currentCell (previousPlayer ps)] (winningPositions (previousPlayer ps)) [currentCell (previousPlayer ps)]) < 999)

-- Generate all valid walls at a game state.
validWalls :: Game -> [Action]
validWalls g = map Place (filter (validWallAction g) walls)
    where 
        walls = concat [[wallRight c, wallTop c] | c<-[(i, j) | i<-allColumns, j<-allRows]]

-- Generate all valid actions at a game state.
validActions :: Game -> [Action]
validActions g = (validSteps g) ++ (validWalls g)

-- Key function. Given a game and an action, checks the validity of the action and applies it to the
-- game, generating a new game.
performAction :: Game -> Action -> Maybe Game
performAction g@(Game b (p:ps)) (Move s)
    | validStepAction g s = 
        Just (Game b (rotatePlayers ((movePlayer (nextTurn p) s):ps)))
    | otherwise = Nothing
performAction g@(Game b (p:ps)) (Place w)
    | validWallAction g w = 
        Just (Game (placeWall b w) (rotatePlayers ((useWall (nextTurn p)):ps)))
    | otherwise = Nothing
