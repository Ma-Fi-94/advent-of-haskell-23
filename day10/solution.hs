{-# LANGUAGE LambdaCase #-}

import Grid
import Data.List (findIndex, elemIndex, intersect)
import Data.Maybe

-----------
-- Sugar --
-----------

type Coord = (Int, Int)

---------------
-- Functions --
---------------

-- Construct a Grid from the input file's lines,
-- using my bespoke Grid module.
makeGrid :: [String] -> Grid Char
makeGrid ss = Grid n k ss
  where
    n = length ss
    k = length . head $ ss


-- Find the coordinates of the starting point,
-- denoted by the character 'S' in the Grid.
-- This is a partial function that will fail,
-- if no 'S' is present. In case of multiple 'S',
-- it will return the first one (LTR-TTB).
findStart :: Grid Char -> (Int, Int)
findStart (Grid _ _ xs) = (i, j)
  where
    i = fromJust . findIndex ('S' `elem`) $ xs
    j = fromJust . elemIndex 'S' $ (xs!!i)


-- Given the grid and the start position, there
-- will be exactly two possible first steps to take.
-- Since it's irrelevant which we take, we'll just
-- take the first one we find (LTR-TTB).
findFirstCell :: Grid Char -> Coord -> Coord
findFirstCell g (i, j)
    |uncurry (cell g) n `elem` "|7F" = n
    |uncurry (cell g) w `elem` "-LF" = w
    |uncurry (cell g) e `elem` "-J7" = e
    |uncurry (cell g) s `elem` "|LJ" = s
  where
    n = (i-1, j)
    w = (i, j-1)
    e = (i, j+1)
    s = (i+1, j)


-- Find all possible next coordinates,
-- given the current Coord and a direction.
nexts :: Coord -> Char -> [Coord]
nexts (i, j) = \case
    '|' -> [n, s]
    '-' -> [e, w]
    'L' -> [n, e]
    'J' -> [n, w]
    '7' -> [s, w]
    'F' -> [s, e]
    _   -> []
  where
    n = (i-1, j)
    w = (i, j-1)
    e = (i, j+1)
    s = (i+1, j)



-- Given a Grid and a history of previously visited
-- Coordinates, go to the Coord of the next cell,
-- returning the updated history.
-- On the way, there will always be two candidates
-- at every step, but one of them has been visited
-- just in the previous step, thus the next step is
-- always unique.
-- When we reach 'S', the will be no places to go,
-- so we stop.
walk :: Grid Char -> [Coord] -> [Coord]
walk g history
    |length nextCoords == 0 = history
    |otherwise              = walk g (history ++ [head nextCoords])
  where
    currentCoord    = last history
    currentSymb     = uncurry (cell g) currentCoord
    candidateCoords = nexts currentCoord currentSymb

    lastCoord       = last . init $ history
    nextCoords      = filter (/=lastCoord) candidateCoords

-------------
-- Answers --
-------------

main = do
    filecontents <- readFile "input.txt"

    let grid      = makeGrid . lines $ filecontents

    let startCell = findStart grid
    let firstCell = findFirstCell grid startCell
    let history   = [startCell, firstCell]

    print $ (`div` 2) . (subtract 1) . length . walk grid $ history

    print $ "---------- Done. ----------"
