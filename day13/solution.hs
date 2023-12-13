import Data.List (transpose)
import Utils (tok, (&&&), second)
import Debug.Trace (trace)

------------ 
-- Part 1 --
------------

-- Check whether a given row is symmetric about the given position.
isSymmAt :: Eq a => Int -> [a] -> Bool
isSymmAt i xs = allSame (zip (reverse left) right)
  where
   allSame       = all (uncurry (==))
   (left, right) = (take i xs, drop i xs)
 
 
-- Find the symmetry column of a block of lines
findSymmCol :: Eq a => [[a]] -> Int
findSymmCol ls = go 1
  where
    go i
        |i == (length (ls!!0)) = 0
        |all (isSymmAt i) ls   = i
        |otherwise             = go (i+1)
 
 
-- Find the symmetry row of a block of lines
findSymmRow :: Eq a => [[a]] -> Int
findSymmRow = findSymmCol . transpose


------------ 
-- Part 2 --
------------

-- Count the number of mismatches if we try to mirror a list
-- after the specified element.
countMismatchesAt :: Eq a => Int -> [a] -> Int
countMismatchesAt i xs = length
                       . filter (uncurry (/=))
                       $ (zip (reverse left) right)
  where
   (left, right) = (take i xs, drop i xs)


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


findSymmRow2 :: Eq a => [[a]] -> Int
findSymmRow2 = findSymmCol2 . transpose

-- Answers
main = do
  
    filecontents <- readFile "input.txt"
    let blocks = tok [""] . lines $ filecontents

    -- Part 1
    --print $ sum
    --      . map (\(u, v) -> u + v)
    --      . map (second (*100))
    --      . map ((&&&) findSymmCol findSymmRow)
    --      $ blocks
    
    -- Part 2
    print $ sum
          . map (\(u, v) -> u + v)
          . map (second (*100))
          . map ((&&&) findSymmCol2 findSymmRow2)
          $ blocks

    

    print $ "---------- Done. ----------"
