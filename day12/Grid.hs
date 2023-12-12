module Grid where

import Data.Maybe
import qualified Data.List as DL

data Grid a = Grid Int Int [[a]] deriving Show


-- Make a Grid from a given list.
-- This is for convenience, and to check if the list of lists is well-formed.
grid :: [[a]] -> Grid a
grid xs
    |wellformed xs = Grid (length xs) (length (xs!!0)) xs
    |otherwise     = error "Grid.grid: Malformed input."
  where
    wellformed ls = all (==length (ls!!0)) (map length ls)


-- Transposes an n*k grid to the corresponding k*n grid.
transpose :: Grid a -> Grid a
transpose (Grid n k xs) = Grid k n (DL.transpose xs)


-- Get the i-th row from the grid. Throws an error when OOB.
row :: Grid a -> Int -> [a]
row (Grid n k xs) i
    |i < n     = xs !! i
    |otherwise = error "Grid.row: Out of bounds."


-- Get the i-th column from the grid. Throws an error when OOB.
col :: Grid a -> Int -> [a]
col g@(Grid n k xs) i
    |i < k     = row (transpose g) i
    |otherwise = error "Grid.col: Out of bounds."


-- Get the element at (i,j). Throws an error when OOB.
cell :: Grid a -> Int -> Int -> a
cell (Grid n k xs) i j
    |i < n && j < k = (xs!!i)!!j
    |otherwise      = error "Grid.cell: Out of bounds." 


-- Uncurried version of cell, to operate on 2-tuples
ucell g = uncurry (cell g)

-- Like 'cell', but returns either 'Just a', or Nothing when OOB.
cellMaybe :: Grid a -> Int -> Int -> Maybe a
cellMaybe (Grid n k xs) i j 
    |i < n && j < k = Just ((xs !! i) !! j)
    |otherwise      = Nothing

-- Get the Moore neighbourhood of cell (i,j). Takes care of boundaries.
moore :: Grid a -> Int -> Int -> [a]
moore g i j = catMaybes neighbours
  where
    coords     = [(i-1, j-1), (i-1, j), (i-1, j+1),
                  (i, j-1),             (i, j+1),
                  (i+1, j-1), (i+1, j), (i+1, j+1)]
    neighbours = map (uncurry (cellMaybe g)) coords


-- Get the von Neumann neighbourhood of cell (i,j). Takes care of boundaries.
vonNeum :: Grid a -> Int -> Int -> [a]
vonNeum g i j = catMaybes neighbours
  where
    coords     = [             (i-1, j),
                  (i, j-1),             (i, j+1),
                               (i+1, j)          ]
    neighbours = map (uncurry (cellMaybe g)) coords

