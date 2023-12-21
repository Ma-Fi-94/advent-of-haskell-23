{-# LANGUAGE LambdaCase #-}

import Control.Arrow (second)
import Data.List (elemIndex, nub)
import Data.Maybe (isJust, fromJust)
import Grid
import Debug.Trace (trace)


----------
-- Both --
----------

-- Replace the single 'S' in the input with a '.'
replaceStart :: [[Char]] -> [[Char]]
replaceStart ss = map (map replace) ss
  where
    replace = \case {'S' -> '.'; x -> x}


-- Find the middle of square Grid with odd length
findStart :: Grid a -> Coord
findStart (Grid l _ _) = (i, i)
  where
    i = (l - 1) `div` 2


-- Given the Grid, find all points marked with '.', i.e.
-- all valid destination points we need to check.
-- For performance, we also preselect only those cells,
-- whose taxicab distance from the start point is smaller
-- than a specific threshold.
findDestinations :: Grid Char -> Coord -> Int -> [Coord]
findDestinations g start thr = map fst
                             . filter ((<thr) . cab start . fst)
                             . filter ((=='.') . snd)
                             . enumerate $ g
  where
    cab (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))


-- Get valid (walkable fields) neighbours in a given
-- Grid of a given coordinate.
getNeighbours :: Grid Char -> Coord -> [Coord]
getNeighbours g c = map fst
               . filter ((=='.') . snd)
               $ vonNeum g c


-- Given a grid and a starting point, find the shortest
-- distance to all given destination points.
bfs :: Grid Char -> Coord -> [Coord] -> [(Coord, Int)]
bfs grid start toFind = go [start] [] 0
  where
    go x y ctr
        |trace (show ctr) False = undefined -- for the impatient (like me)
    go []    founds _   = founds
    go queue founds ctr = go queue' founds' (ctr+1)
      where
        founds'          = founds ++ (zip queue (repeat ctr))
        queue'           = allNewNeighbours
        allNewNeighbours = filter (`elem` toFind)               -- filter out those we don't even need to find
                         . filter (`notElem` (map fst founds))  -- filter out those we already found
                         . nub                                  -- Discard duplicates
                         $ concatMap (getNeighbours grid) queue -- list of all neighbours of all cells in queue


------------
-- Part 2 --
------------

-- Append the specified number of copies of the given input
-- into all directions.
appendN :: Int -> [[a]] -> [[a]]
appendN n rows = concat $ replicate dim rows' 
  where
    dim   = 2 * n + 1
    rows' = map (concat . replicate dim) rows


main = do

    ------------
    -- Part 1 --
    ------------

    input       <- lines <$> readFile "input.txt"
    let grid    = Grid.fromList . replaceStart $ input
    let start   = findStart grid
    let finishs = findDestinations grid start 65

    -- For every walkable cell, we get the minimum result from the start
    let bfsResult = bfs grid start finishs

    -- Now we retain only those cells that have a minimum distance <=
    -- the given threshold, and an even number of steps.
    -- The idea here is that a cell with e.g. distance 62 can also be
    -- reached with 62 steps plus one additional step plus one step back. 
    print $ length
          . filter (even . snd)
          . filter ((<=64) . snd)
          $ bfsResult

    
    ------------
    -- Part 2 --
    ------------

    -- Note the the rown and column of the starting point, as well
    -- as the border of the patch are entirely free. Hence, the number
    -- of fields we can visit should scale quadratically in k, where
    -- k denotes the number of the patch. For a polynomial of degree 2,
    -- we will need 3 points, such as 65 + k*131, k = 0, 1, 2.

    -- First, we extend the given patch three times in every direction,
    -- then calculate BFS on it. Because we only need the numbers up to
    -- 65 + 2*131 steps, we can safely cap the set of finishing points
    -- to evaluate at the taxicab metric below this number plus some padding.
    -- It still takes a lot of time :(.
    input2         <- lines <$> readFile "input.txt"
    let grid2      = Grid.fromList . replaceStart . appendN 3 $ input2
    let start2     = findStart grid2
    let finishs2   = findDestinations grid2 start2 (65 + 2*131 + 2)
    let bfsResult2 = bfs grid2 start2 finishs2
   
    -- We then calculate the number at 65 + k*131 for k = 0, 1, 2.
    -- Importantly, we need to adapt the last filtering step,
    -- depending on the parity!

    -- f(k=0) = 3770
    print $ length
          . filter (odd . snd)
          . filter ((<=65) . snd)
          $ bfsResult2

    -- f(k=1) = 33665
    print $ length
          . filter (even . snd)
          . filter ((<=(65 + 131)) . snd)
          $ bfsResult2
    
    -- f(k=2) = 93356
    print $ length
          . filter (odd . snd)
          . filter ((<=(65 + 2 * 131)) . snd)
          $ bfsResult2

    -- To these, we fit f(k) = 14898 k^2 + 14997 k + 3770. It only remains
    -- to evaluate f at k' = (26501365 - 65) / 131 = 202300. We get
    -- f(k') = 609708004316870, which is our answer.
    let k' = 202300
    print $ 14898 * k'^2 + 14997 * k' + 3770
   
    
    print $ "---------- Done. ----------"



