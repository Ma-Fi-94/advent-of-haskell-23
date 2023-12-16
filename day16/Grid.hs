module Grid where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (catMaybes)
import qualified Data.List as DL (transpose)

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


-- Transpose the Grid
transpose :: Grid a -> Grid a
transpose = fromList . DL.transpose . toList


-- Enumerate all elements as a list of 2-tuples (Coordinate, Element)
enumerate :: Grid a -> [(Coord, a)]
enumerate (Grid h w m) = zip idx $ map (m Map.!) idx
  where
    idx = (,) <$> [0..h-1] <*> [0..w-1]


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


-- Get a row, specified by index. Throws error if OOB.
row :: Grid a -> Int -> [a]
row g i = case mbRow g i of
               Just x  -> x
               Nothing -> error "Grid.row: Out of bounds."


-- Get a column, specified by index. Throws error if OOB.
col :: Grid a -> Int -> [a]
col g i = case mbCol g i of 
               Just x  -> x
               Nothing -> error "Grid.col: Out of bounds."


-- Get a cell, specified by a Coordinate tuple. Throws error if OOB.
cell :: Grid a -> Coord -> a
cell g c = case mbCell g c of
                Just x  -> x
                Nothing -> error "Grid.cell: Out of bounds."


-- Change a given cell to a given value.
-- Returns a Grid, or throws error if OOB.
set :: Grid a -> Coord -> a -> Grid a
set (Grid h w m) (r, c) x
    |0 <= r && r < h && 0 <= c && c < w = g'
    |otherwise                          = error "Grid.set: Out of bounds."
      where
        g' = Grid h w m'
        m' = Map.insert (r, c) x m



-- Get the Moore neighbourhood of cell (i,j). Takes care of boundaries.
moore :: Grid a -> Coord -> [a]
moore g (i, j) = catMaybes neighbours
  where
    coords     = [(i-1, j-1), (i-1, j), (i-1, j+1),
                  (i, j-1),             (i, j+1),
                  (i+1, j-1), (i+1, j), (i+1, j+1)]
    neighbours = map (mbCell g) coords


-- Get the von Neumann neighbourhood of cell (i,j). Takes care of boundaries.
vonNeum :: Grid a -> Coord -> [a]
vonNeum g (i, j) = catMaybes neighbours
  where
    coords     = [          (i-1, j),
                  (i, j-1),           (i, j+1),
                            (i+1, j)          ]
    neighbours = map (mbCell g) coords

