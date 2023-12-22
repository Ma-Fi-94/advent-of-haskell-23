module Grid where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (isJust, fromJust)
import Control.Arrow (second)

type Coord = (Int, Int)

data Grid a = Grid Int Int (Map Coord a) deriving Show


-- Make a Grid from a given list of lists.
-- Checks whether input is wellformed.
fromList :: [[a]] -> Grid a
fromList ls
    |wellformed = Grid h w m
    |otherwise  = error "Grid.fromList: Malformed input."
      where
        wellformed = all (==length (ls!!0)) (map length ls)
        (h, w)     = (length ls, length (ls!!0))
        m          = Map.fromList $ zip idx (concat ls)
        idx        = (,) <$> [0..h-1] <*> [0..w-1]


-- Return all elements list-of-lists style.
toList :: Grid a -> [[a]]
toList g@(Grid h _ _) = map (row g) [0..h-1]


-- Return all elements as a list of 2-tuples (Coordinate, Element)
enumerate :: Grid a -> [(Coord, a)]
enumerate (Grid _ _ m) = Map.assocs m



-- Transpose the Grid
transpose :: Grid a -> Grid a
transpose (Grid h w m) = Grid h' w' m'
  where
    (h', w') = (w, h)
    m'       = Map.mapKeys (\(r, c) -> (c, r)) m


-- Reverse all rows of the Grid
revRows :: Grid a -> Grid a
revRows (Grid h w m) = Grid h w m'
  where
    m' = Map.mapKeys (\(r, c) -> (r, w - c - 1)) m



-- Get a Maybe row, specified by index. Returns Nothing if OOB.
mbRow :: Grid a -> Int -> Maybe [a]
mbRow (Grid h w m) i
    |0 <= i && i < h = Just $ map (m Map.!) $ [(i, x) | x <- [0..w-1]]
    |otherwise       = Nothing


-- Get a Maybe column, specified by index. Returns Nothing if OOB.
mbCol :: Grid a -> Int -> Maybe [a]
mbCol (Grid h w m) i
    |0 <= i && i < w = Just $ map (m Map.!) $ [(x, i) | x <- [0..h-1]]
    |otherwise       = Nothing


-- Get a Maybe cell, specified by a Coordinate tuple. Returns Nothing if OOB.
mbCell :: Grid a -> Coord -> Maybe a
mbCell (Grid h w m) (r, c)
    |0 <= r && r < h && 0 <= c && c < w = Just $ m Map.! (r, c)
    |otherwise                          = Nothing


-- Change a given cell to a given value.
-- Returns a Just Grid, or Nothing if OOB.
mbSet :: Grid a -> Coord -> a -> Maybe (Grid a)
mbSet (Grid h w m) (r, c) x
    |0 <= r && r < h && 0 <= c && c < w = Just g'
    |otherwise                          = Nothing
      where
        g' = Grid h w m'
        m' = Map.insert (r, c) x m


-- A custom fromJust implementation, that allows to
-- specify the error to be thrown in case of Nothing.
-- This is from the partial versions of the mb* functions.
fromJust' :: String -> Maybe a -> a
fromJust' err mb = case mb of
                        Just x  -> x
                        Nothing -> error err


-- Partial versions of the mb* functions, for convenience.
row g i  = fromJust' "Grid.row: Out of bounds." $ mbRow g i
col g i  = fromJust' "Grid.col: Out of bounds." $ mbCol g i
cell g i = fromJust' "Grid.cell: Out of bounds." $ mbCell g i
set g c x = fromJust' "Grid.set: Out of bounds." $ mbSet g c x


-- Get the Moore neighbourhood of cell (i,j). Takes care of boundaries.
moore :: Grid a -> Coord -> [(Coord, a)]
moore g (i, j) = map (second fromJust) $ filter (isJust . snd) neighbours
  where
    coords     = [(i-1, j-1), (i-1, j), (i-1, j+1),
                  (i, j-1),             (i, j+1),
                  (i+1, j-1), (i+1, j), (i+1, j+1)]
    neighbours = zip coords $ map (mbCell g) coords


-- Get the von Neumann neighbourhood of cell (i,j). Takes care of boundaries.
vonNeum :: Grid a -> Coord -> [(Coord, a)]
vonNeum g (i, j) = map (second fromJust) $ filter (isJust . snd) neighbours
  where
    coords     = [          (i-1, j),
                  (i, j-1),           (i, j+1),
                            (i+1, j)          ]
    neighbours = zip coords $ map (mbCell g) coords

