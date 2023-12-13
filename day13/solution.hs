import Data.List (transpose)
import Utils (tok, (&&&))


----------
-- Both --
----------

-- Count the number of mismatches if we try to mirror a list
-- after the specified element.
countMismatchesAt :: Eq a => Int -> [a] -> Int
countMismatchesAt i xs = length
                       . filter (uncurry (/=))
                       $ (zip (reverse left) right)
  where
   (left, right) = (take i xs, drop i xs)


-- Analyse a Char block, given a "symmetry-finding function" to apply.
solveBlock :: Eq a => ([[a]] -> Int) -> [[a]] -> Int
solveBlock f = uncurry (+) . (&&&) f ((*100) . f. transpose)


------------ 
-- Part 1 --
------------
 
-- Find the symmetry column of a block of lines
findSymmCol :: Eq a => [[a]] -> Int
findSymmCol ls = go 1
  where
    go i
        |i == (length (ls!!0))                    = 0
        |all (==0) $ map (countMismatchesAt i) ls = i
        |otherwise                                = go (i+1)
 

------------ 
-- Part 2 --
------------

-- Now we want to find the column, around which the table
-- is symmetric except for exactly one mismatch (the "smudge")
findSymmCol2 :: Eq a => [[a]] -> Int
findSymmCol2 ls = go 1
  where
    go i
        -- |trace (show i ++ ", " ++ show nbsMism) False = 77777
        |i == (length (ls!!0)) = 0
        |found                 = i
        |otherwise             = go (i+1)
      where
        found    = length (filter (==1) nbsMism) == 1
                && length (filter (==0) nbsMism) == length nbsMism - 1
        nbsMism  = map (countMismatchesAt i) ls


-- Answers
main = do
  
    filecontents <- readFile "input.txt"
    let blocks = tok [""] . lines $ filecontents

    -- Part 1
    print $ sum . map (solveBlock findSymmCol) $ blocks
    
    -- Part 2
    print $ sum . map (solveBlock findSymmCol2) $ blocks

    print $ "---------- Done. ----------"
