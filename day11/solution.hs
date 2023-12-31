import Data.List (elemIndices, (\\), sortOn)


-- Sugar
type Coord = (Int, Int)


-- Uncurried taxicab metric
cab :: (Coord, Coord) -> Int
cab ((x1, y1), (x2, y2)) = abs (x1-x2) + abs (y1-y2)


-- All pairs of the elements of a list
pairs :: Eq a => [a] -> [(a,a)]
pairs xs = [(x1, x2) | x1 <- xs, x2 <- xs]


-- Converts input file's lines to a list of coordinates.
-- We do this in the very beginning for optimal performance.
lines2coords :: [String] -> [Coord]
lines2coords = go 0
  where
    go _ []     = []
    go i (l:ls) = (map (\c -> (i,c)) (elemIndices '#' l)) ++ go (i+1) ls


-- Expand along both axes by applying expandRows on the grid
-- and afterwards on the transposed (and sorted) grid again.
expandN :: Int -> [Coord] -> [Coord]
expandN n = transpose . expandRowsN n . transpose . expandRowsN n
  where
    transpose = sortOn fst . map (\(x,y) -> (y,x))


-- Expand the "grid" row-wise by adding in 'n' new rows per empty row.
expandRowsN :: Int -> [Coord] -> [Coord]
expandRowsN n = go 0 0
  where
    go _ _ []            = []
    go idx offset coords = if   null currentLine
                           then                 go (idx+1) (offset+n) coords
                           else currentLine' ++ go (idx+1) offset     coords'
      where
        currentLine  = filter ((== idx) . fst) coords
        currentLine' = map (\(x, y) -> (x + offset, y)) currentLine
        coords'      = coords \\ currentLine


-------------
-- Answers --
-------------

main = do
  
    filecontents <- readFile "input.txt"
    let coords = lines2coords . lines $ filecontents

    -- Part 1
    print $ (`div` 2) . sum . map cab . pairs . expandN 1 $ coords

    -- Part 2
    print $ (`div` 2) . sum . map cab . pairs . expandN (1000000-1) $ coords


    print $ "---------- Done. ----------"
