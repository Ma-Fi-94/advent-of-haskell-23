{-# LANGUAGE LambdaCase #-}

import Control.Arrow (second)
import Data.List (elemIndex, nub)
import Data.Maybe (isJust, fromJust)
import Grid

import Debug.Trace (trace)

-- Find the one single 'S' in the input list of Strings.
findStart :: [[Char]] -> Coord
findStart = head
          . map (second fromJust)
          . filter (isJust . snd)
          . zip [0..]
          . map (elemIndex 'S')


-- Replace the single 'S' in the input with a '.'
replaceStart :: [[Char]] -> [[Char]]
replaceStart ss = map (map replace) ss
  where
    replace = \case {'S' -> '.'; x -> x}


-- Given the Grid, find all points marked with '.', i.e.
-- all valid destination points we need to check.
findDestinations :: Grid Char -> Coord -> [Coord]
findDestinations g start = map fst
                         . filter ((=='.') . snd)
                         . enumerate $ g


-- Get valid (walkable fields) neighbours in a given Grid of a given coordinate.
getNeighbours :: Grid Char -> Coord -> [Coord]
getNeighbours g c = map fst
               . filter ((=='.') . snd)
               $ vonNeum g c


-- Given a grid and a starting point, find the shortest distance to all
-- given destination points.
bfs :: Grid Char -> Coord -> [Coord] -> [(Coord, Int)]
bfs grid start toFind = go [start] [] 0
  where
    go []    founds _   = founds
    go queue founds ctr = go queue' founds' (ctr+1)
      where
        founds'          = founds ++ (zip queue (repeat ctr))
        queue'           = allNewNeighbours
        allNewNeighbours = filter (`elem` toFind)               -- filter out these we don't even need to find
                         . filter (`notElem` (map fst founds))  -- filter out these we already found
                         . nub                                  -- Discard duplicates
                         $ concatMap (getNeighbours grid) queue -- list of all neighbours of all cells in queue


main = do
    input <- lines <$> readFile "input.txt"
    let grid      = Grid.fromList . replaceStart $ input
    let start     = findStart input
    let finishs   = findDestinations grid start

    ------------
    -- Part 1 --
    ------------

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

    print $ "---------- Done. ----------"



