-- This is heavily based on the following repo:
-- https://github.com/gruhn/advent-of-code/blob/master/2023/Day17.hs
-- I only did some minor editing and reformatting, and added comments
-- to aid my understanding, but all credit goes to the original solution.

{-# LANGUAGE LambdaCase #-}

module Main where

import Algorithm.Search (dijkstra)
import Control.Monad (guard)
import Control.Arrow (first, second)
import Data.Char (digitToInt)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

-- Sugar
type Pos = (Int, Int)
data Dir = N | S | W | E deriving (Eq, Ord, Show)
type State = (Pos, [Dir]) -- current postions and options for next move


-- Parse input lines to a Map from Pos to Int
parseToMap :: [String] -> Map Pos Int
parseToMap ss = Map.fromList
              . zip idx
              . concat
              . map (map digitToInt)
              $ ss
  where
    idx    = (,) <$> [0..h-1] <*> [0..w-1]
    (h, w) = (length ss, length (ss!!0))


-- Move from a given position i steps into the given direction.
move :: Pos -> Int -> Dir -> Pos
move pos i = \case
              N -> second (subtract i) pos
              S -> second (+i) pos
              W -> first (subtract i) pos
              E -> first (+i) pos


-- Options to move after a step in a given direction
moveOptions :: Dir -> [Dir]
moveOptions = \case
              N -> [W,E]
              S -> [W,E]
              W -> [N,S]
              E -> [N,S]


-- Given to positions with nonzero distance, generate all coordinates
-- of points between them (excluding the last one).
path :: Pos -> Pos -> [Pos]
path (x1,y1) (x2,y2)
    |(x1,y1) == (x2,y2) = []
    |otherwise          = pos : path pos (x2,y2)
      where
        pos = (x1 + signum (x2-x1), y1 + signum (y2-y1))


shortestPath :: Map Pos Int -> [Int] -> Maybe Int
shortestPath grid step_range = fst <$> dijkstra next cost isGoal ((0,0), [E,S])
  where
      -- Is the provided position the goal?
      isGoal (pos, _) = pos == goal
          where
            goal = fst $ Map.findMax grid

      -- The cost function for moving between two states.
      -- Includes start point, excludes last point to prevent doublecounting.
      cost (from, _) (to, _) = sum $ map (grid Map.!) $ path from to

      -- Given a State, find all possible next States
      next :: State -> [State]
      next ((x,y), dir_options) = do 
        steps <- step_range
        dir   <- dir_options
        let new_pos = move (x,y) steps dir
        guard $ new_pos `Map.member` grid
        return (new_pos, moveOptions dir)



main = do
  grid <- parseToMap . lines <$> readFile "input.txt"

  print $ fromJust $ shortestPath grid [1..3]

  print $ fromJust $ shortestPath grid [4..10]