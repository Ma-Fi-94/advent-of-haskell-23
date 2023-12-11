{-# LANGUAGE LambdaCase #-}

import Data.List (elemIndices, (\\), sortOn)
-----------
-- Sugar --
-----------

type Coord = (Int, Int)


---------------
-- Functions --
---------------

-- Uncurried taxicab metric
cab :: (Coord, Coord) -> Int
cab ((x1, y1), (x2, y2)) = abs (x1-x2) + abs (y1-y2)


-- Extract coordinates of non-empty cells iteratively row by row
lines2coords :: [String] -> [Coord]
lines2coords = go 0
  where
    go _ []     = []
    go i (l:ls) = (map (\c -> (i,c)) (elemIndices '#' l)) ++ go (i+1) ls


-- Expand the "grid" row-wise by adding in 'n' new rows per empty row.
-- We operate on the list of coordinates already, as this
-- is more efficient than working with a character grid in memory.
expandRowsN :: Int -> [Coord] -> [Coord]
expandRowsN n = go 0 0
  where
    go _ _ []            = []
    go idx offset coords = if   null currentLine
                           then                 go (idx+1) (offset+n) coords
                           else currentLine' ++ go (idx+1) offset     coords'
      where
        currentLine  = filter (\(x, _) -> x == idx) coords
        currentLine' = map (\(x, y) -> (x + offset, y)) currentLine
        coords'      = coords \\ currentLine


-- Expand along both axes by applying expandRows on the grid
-- and afterwards on the transposed (and sorted) grid again.
expandN :: Int -> [Coord] -> [Coord]
expandN n = transpose . expandRowsN n . transpose . expandRowsN n
  where
    transpose = sortOn fst . map (\(x,y) -> (y,x))


-------------
-- Answers --
-------------

main = do
  
    filecontents <- readFile "input.txt"
    let coords = lines2coords . lines $ filecontents

    ------------
    -- Part 1 --
    ------------

    let coords1 = expandN 1 coords
    let pairs1  = [(c1, c2) | c1 <- coords1,
                              c2 <- coords1,
                              c1 /= c2]
    print $ (`div` 2) . sum . map cab $ pairs1

    ------------
    -- Part 2 --
    ------------


    let coords2 = expandN (1000000-1) coords
    let pairs2  = [(c1, c2) | c1 <- coords2,
                              c2 <- coords2,
                              c1 /= c2]
    print $ (`div` 2) . sum . map cab $ pairs2

    print $ "---------- Done. ----------"