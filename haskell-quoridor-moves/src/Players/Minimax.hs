{-
    Module: Minimax.

    *** PART I (60pt) and PART II (10pt) *** 
-}
module Players.Minimax where 

import Data.Maybe
import Data.Graph
import Data.Ord
import Data.Tree
import Data.List
import Data.Array

import Types
import Constants
import Cell
import Action
import Board 
import Player
import Game
import Players.Dumb (dumbAction)

{-
    StateTree util.
-}

-- Map a function through the nodes of the tree.
mapStateTree :: (v -> w) -> StateTree v a -> StateTree w a
mapStateTree f (StateTree x ts) = StateTree (f x) [(a, mapStateTree f t) | (a, t)<-ts]

-- Calculate the depth of the tree (used to test pruneDepth).
stateTreeDepth :: StateTree v a -> Int 
stateTreeDepth (StateTree _ []) = 0
stateTreeDepth (StateTree _ ts) = 1 + (maximum (map (stateTreeDepth . snd) ts))

-- Calculate the breadth of the tree (used to test pruneBreadth).
stateTreeBreadth :: StateTree v a -> Int
stateTreeBreadth (StateTree _ []) = 0
stateTreeBreadth (StateTree _ ts) = max (length ts) (maximum (map (stateTreeBreadth . snd) ts))

{-
    Result util.
-}

-- Negating the result is simply negating the score. You may ignore this although it may be useful
-- to implement the minimax algorithm.
negResult :: Result -> Result
negResult (Result x as) = Result (-x) as




{- 
    * *** Part I.a (10pt) ***

    First, we will generate a tree containing all the possible game states.
-}

-- Given a game, return a tree that encodes all the possible future game states.
-- [Hint: Use 'validActions' and 'performAction'.]
-- [Note: To speed things up, you may want to, at this stage, heuristically select which actions are 
--  more relevant. In particular, you probably don't want to consider every single possible wall.]

-- * DONE works in every case.

generateGameTree :: Game -> GameTree
generateGameTree g = StateTree g [ (a, (generateGameTree (fromJust(performAction g a))) ) | a <- validActions g ]



{-
    * *** PART I.b (5pt) ***

    Re-order the tree so that when traversed by the minimax algorithm, when it traverses the 
    branches at each node, finds either the higher scores or the lower scores first, depending on
    the depth of the tree.
-}

-- * DONE in all cases

-- * this is the first version of the cw, where we alternate between the two orderings
-- Higher scoring nodes go first.
-- [Hint: You should use 'lowFirst'.]
highFirst' :: (Ord v) => StateTree v a -> StateTree v a
highFirst' (StateTree v branches) = StateTree v [ (a, lowFirst' stt) | (a, stt) <- sortBy f branches]
                                                    where f (a, (StateTree v1 br1)) (b, (StateTree v2 br2)) = (flip compare) v1 v2


-- Lower scoring nodes go first.
-- [Hint: You should use 'highFirst'.]
lowFirst' :: (Ord v) => StateTree v a -> StateTree v a
lowFirst' (StateTree v branches) = StateTree v [ (a, highFirst' stt) | (a, stt) <- sortBy f branches]
                                                    where f (a, (StateTree v1 br1)) (b, (StateTree v2 br2)) = compare v1 v2


-- * this was the second iteration of the cw, where we could only focus on ordering from high to low (and hence the tests pass):
highFirst :: (Ord v) => StateTree v a -> StateTree v a
highFirst (StateTree v branches) = StateTree v [ (a, highFirst stt) | (a, stt) <- sortBy f branches]
                                                    where f (a, (StateTree v1 br1)) (b, (StateTree v2 br2)) = (flip compare) v1 v2

lowFirst :: (Ord v) => StateTree v a -> StateTree v a
lowFirst (StateTree v branches) = StateTree v [ (a, lowFirst stt) | (a, stt) <- sortBy f branches]
                                                    where f (a, (StateTree v1 br1)) (b, (StateTree v2 br2)) = compare v1 v


{-
    * *** Part I.c (5pt) ***

    We don't want to look at all possible future game states as that would consume too much time and
    memory. Instead, we will only look a given number of steps into the future. Formally, the future
    game states are encoded in a tree, so we need a function that reduces the depth of a tree.
-}

-- Given a depth and a tree, return the same tree but cutting off the branches when the depth is 
-- exceeded. 
-- [Hint: You may want to use guards and recursion.]

-- * DONE works in every scenario

pruneDepth :: Int -> StateTree v a -> StateTree v a
pruneDepth 0 (StateTree g branches) = StateTree g []
pruneDepth l (StateTree g branches) = StateTree g [ (a, pruneDepth (l-1) stt) | (a, stt) <- branches] 




{-
    * *** Part I.d (5pt) ***

    Similarly, we can also make our tree smaller by not considering all the possible game states at
    a given point. We need a function that reduces the breadth (or width) of a tree.
-}

-- Given a breadth (Int n) and a tree, return the same tree but only keeping the first n branches at
-- every node. 
-- [Hint: Use 'take'.]

-- * DONE works in every scenario

pruneBreadth :: Int -> StateTree v a -> StateTree v a
pruneBreadth n (StateTree g branches) = StateTree g [ (a, pruneBreadth n stt) | (a, stt) <- (take n branches) ]



{-
    * *** Part I.e (15pt) ***

    A crucial part of the minimax algorithm is defining a good utility function. It should measure
    how good a game position is for the current player. In our case, a game state should be better
    than another one if the player is closer to its winning positions.
-}

-- Assign a value to each game (from the point of view of the current player).
-- [Hint 1: You may want to calculate the distance between the player's current cell and its winning
--  positions.]
-- [Hint 2: One way would be to use 'reachableCells' repeatedly.]

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates items = helper [] items
    where helper seen [] = seen
          helper seen (x:xs)
              | x `elem` seen = helper seen xs
              | otherwise = helper (seen ++ [x]) xs

-- * returns the shortest distance from the current cell to the winning cells,
distWinning :: Board -> [Cell] -> [Cell] -> [Cell] -> Int
distWinning bd [] wc visC = 999  -- this means that there is no path to the winning cells
distWinning bd cTE wc visC
    | helper cTE wc    = 0
    | otherwise        = 1 + distWinning bd (removeDuplicates reachableC) wc (visC ++ (removeDuplicates reachableC))

        where
            helper [] b = False
            helper (x:xs) b = (x `elem` b) || helper xs b

            reachableC = foldr (++) [] [[ newCell | newCell <- reachableCells bd prevCell, not (newCell `elem` visC)] | prevCell <- cTE]

-- ! this is a utility as we have seen them in the lectures. Hence, the tests don't pass for minimax-tests
utility :: Player -> Game -> Int 
utility playerMAX (Game b ps)
    | playerMAX `theSamePlayer` (currentPlayer ps)  =  (distWinning b [currentCell (previousPlayer ps)] (winningPositions (previousPlayer ps)) [currentCell (previousPlayer ps)]) - ((distWinning b [currentCell (currentPlayer ps)] (winningPositions (currentPlayer ps)) [currentCell (currentPlayer ps)]))
    | otherwise                                     =  (distWinning b [currentCell (currentPlayer ps)] (winningPositions (currentPlayer ps)) [currentCell (currentPlayer ps)]) - ((distWinning b [currentCell (previousPlayer ps)] (winningPositions (previousPlayer ps)) [currentCell (previousPlayer ps)]))
            where
                -- identify players by winning positions
                plr1 `theSamePlayer` plr2 = (winningPositions plr1) == (winningPositions plr2)


-- Lifting the utility function to work on trees.
evalTree :: Player -> GameTree -> EvalTree 
evalTree playerMax = mapStateTree (utility playerMax)




{-
    * *** Part I.f (20pt) ***

    Finally, we ask you to implement the minimax algorithm. Given an evaluation tree, it should 
    return the a high scoring action (according to the minimax algorithm).
-}

-- Given an evaluation tree (it stores a score in the node and each branch is labelled with the 
-- action that leads to the next child) return a list of actions
-- [Hint 1: Use a helper function to keep track of the highest and lowest scores.]
-- [Hint 2: Use the 'Result' datatype.]

-- ! correct implementation of minimax according to lectures

minimaxFromTree :: EvalTree -> Action
minimaxFromTree (StateTree v branches) = decideActionTOP branches

decideActionTOP :: [(Action, StateTree Int Action)] -> Action
decideActionTOP branches = fst (foldr maxi (Move (('a', 1), ('b', 1)), -99999) [ (a, decideValueBOTmin tree)  | (a, tree) <- branches])
                                where
                                    maxi (a, b) (c, d)
                                        | b > d = (a, b)
                                        | otherwise = (c, d)

decideValueBOTmin :: EvalTree -> Int
decideValueBOTmin (StateTree v []) = v
decideValueBOTmin (StateTree v branches) = minimum [ decideValueBOTmax tree | (a, tree) <- branches]

decideValueBOTmax :: EvalTree -> Int
decideValueBOTmax (StateTree v []) = v
decideValueBOTmax (StateTree v branches) = maximum [ decideValueBOTmin tree | (a, tree) <- branches]

{-
    * *** Part II (10pt) ***

    Extension of Part I.e, using alpha-beta pruning. You will need to change the 'minimax' function
    below to make it use this function.
-}

-- Same as above but now use alpha-beta pruning.
-- [Hint 1: Extend the helper function in I.e to keep track of alpha and beta.]
-- [Hint 2: Use the 'Result' datatype.]

-- ! works in the implementation made from our lectures

minimaxABFromTree :: EvalTree -> Action
minimaxABFromTree tree = action
                        where
                            (Result _ [action]) = maxValueBOT (-999) 999 (Result (-999) []) tree


maxValueBOT :: Int -> Int -> Result -> EvalTree -> Result
maxValueBOT _ _ _ (StateTree v []) = Result v []      -- case when we are at the leaves of the eval tree

maxValueBOT a b best@(Result maxi bestAct) (StateTree v ((act, tree):[]) )     -- case when we are at the end of a branch but not at a leaf
                        | c >= maxi    = Result c [act]
                        | otherwise    = best
                            where
                                (Result c _ ) = (minValueBOT a b (Result 999 []) tree)

maxValueBOT a b (Result maxi bestAct) (StateTree val ((act, tree):rest) )
                        | v >= b                    = bestResult
                        | otherwise                 = maxValueBOT (max a v) b bestResult (StateTree val rest)
                            where
                                (Result c _ ) = (minValueBOT a b (Result 999 []) tree)
                                v = max maxi c
                                bestResult = (Result v (helper (maxi, bestAct) (c, [act])))
                                    where
                                        helper (a, b) (c, d)
                                            | a > c     = b
                                            | otherwise = d

minValueBOT :: Int -> Int -> Result -> EvalTree -> Result
minValueBOT _ _ _ (StateTree v []) = Result v []      -- case when we are at the leaves of the eval tree

minValueBOT a b best@(Result mini bestAct) (StateTree v ((act, tree):[]) )        -- case when we are at the end of a branch but not at a leaf
                        | c <= mini    = Result c [act]
                        | otherwise   = best
                            where
                                (Result c _ ) = (maxValueBOT a b (Result (-999) []) tree)

minValueBOT a b (Result mini bestAct) (StateTree val ((act, tree):rest) )
                        | v <= a                    = bestResult
                        | otherwise                 = minValueBOT a (min b v) bestResult (StateTree val rest)
                            where
                                (Result c _ ) = (maxValueBOT a b (Result (-999) []) tree)
                                v = min mini c
                                bestResult = (Result v (helper (mini, bestAct) (c, [act])))
                                    where
                                        helper (a, b) (c, d)
                                            | a < c      = b
                                            | otherwise  = d

{-
    Putting everything together.
-}

-- Given depth for pruning (should be even).
depth :: Int 
depth = 2

-- Given breadth for pruning.
breadth :: Int 
breadth = 25


-- Function that combines all the different parts implemented in Part I.
minimax :: Player -> Game -> Action
minimax playerMAX game = minimaxABFromTree (pruneBreadth breadth (highFirst' (evalTree playerMAX (pruneDepth depth (generateGameTree game) ))))
-- ! change first method to minimaxABFromTree to test AB pruning, and minimaxFromTree for normal minimax


-- Given a game state, calls minimax and returns an action.
-- V: this receives the current board, where the minimax should play, and the list of players has the minimax as current player
minimaxAction :: Board -> [Player] -> String -> Int -> Maybe Action
minimaxAction b ps _ r = let g = Game b ps in minimaxAction' g (minimax (currentPlayer ps) g)
    where 
        -- Goes through the list of actions until it finds a valid one. 
        minimaxAction' :: Game -> Action -> Maybe Action
        minimaxAction' g' (Move s)
            | validStepAction g' s = Just (Move s)
            | otherwise = error "Minimax chose an invalid action."
        minimaxAction' g' (Place w)
            | validWallAction g' w = Just (Place w)
            | otherwise = error "Minimax chose an invalid action."

-- Make minimaxPlayer in the usual way using 'minimaxAction'.
makeMinimaxPlayer :: String -> Cell -> Int -> [Cell] -> Player
makeMinimaxPlayer n c rws wps = Player {
    name = n,
    turn = 1,
    currentCell = c, 
    remainingWalls = rws,
    winningPositions = wps,
    isHuman = False,
    chooseAction = minimaxAction }
