{-# LANGUAGE LambdaCase #-}

import Grid 
import Data.List (findIndex, elemIndex, intersect)
import Data.Maybe
import Utils (first, second)

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
findFirstCell g c
    |ucell g n `elem` "|7F" = n
    |ucell g w `elem` "-LF" = w
    |ucell g e `elem` "-J7" = e
    |ucell g s `elem` "|LJ" = s
  where
    n = first pred $ c
    s = first succ $ c
    w = second pred $ c
    e = second succ $ c

-- Find all possible next coordinates,
-- given the current Coord and a direction.
nexts :: Coord -> Char -> [Coord]
nexts c = \case
    '|' -> [n, s]
    '-' -> [e, w]
    'L' -> [n, e]
    'J' -> [n, w]
    '7' -> [s, w]
    'F' -> [s, e]
    _   -> []
  where
    n = first pred $ c
    s = first succ $ c
    w = second pred $ c
    e = second succ $ c


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
walk grid history
    |length nextCoords == 0 = history
    |otherwise              = walk grid (history ++ [head nextCoords])
  where
    currentCoord    = last history
    currentSymb     = uncurry (cell grid) currentCoord
    candidateCoords = nexts currentCoord currentSymb

    lastCoord       = last . init $ history
    nextCoords      = filter (/=lastCoord) candidateCoords

-----------------
-- Part 2 only --
-----------------

-- Apply the Trapezoid rule to a list of coordinates,
-- to get the area of the polygon.
-- We do not have to filter out non-vertices, since they
-- will lead to zero-summands anyway.
-- We do need to use `abs` as the sum might be negative.
-- We need the first coordinate appear at the end of the list.
area :: [Coord] -> Int
area cs = abs . (`div` 2) . sumOfProducts $ cs
  where
    sumOfProducts (_:[])                  = 0
    sumOfProducts ((y1,x1):c2@(y2,x2):cs) = p + sumOfProducts (c2:cs)
      where
        p = (y1 + y2) * (x1 - x2)

-------------
-- Answers --
-------------

main = do
    filecontents <- readFile "input.txt"
    let grid      = makeGrid . lines $ filecontents
    let startCell = findStart grid
    let firstCell = findFirstCell grid startCell
    let history   = [startCell, firstCell]

    ------------
    -- Part 1 --
    ------------
    
    -- Walk the path, getting all coordinates of the boundary
    let boundary = walk grid $ history

    -- Answer to part 1 is just (l-1)/2
    print $ (`div` 2) . (subtract 1) . length $ boundary

    ------------
    -- Part 2 --
    ------------

    -- We will use Pick's Theorem, which relates
    -- * the area 'a' of a simple polygon with integer-coordinate vertices to
    -- * the number 'b' of points with integer coordinates on the boundary and
    -- * the number 'i' of points with integer coordinates within it.
    -- It holds: a = i + (b/2) - 1, hence i = a - (b/2) + 1.

    -- To determine a, we use the Trapezoid formula, which states:
    -- a = | 0.5 * \sum_{i=1}^n (y_i+y_{i+1})(x_i-x_{i+1}) |
    let a = area $ boundary

    -- Number of integer-coordinate points on the border
    let b = (length boundary) - 1

    -- Application of Pick's Theorem, as described above.
    print $ a - (b `div` 2) + 1

    print $ "---------- Done. ----------"
