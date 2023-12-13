import Data.List (transpose)
import Utils (tok, (&&&), second)
 

-- Check whether a given row is symmetric about the given position.
isSymmAt :: Eq a => Int -> [a] -> Bool
isSymmAt i xs = allSame (zip (reverse left) right)
  where
   allSame       = all (\(u,v) -> u == v)
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


-- Answers
main = do
  
    filecontents <- readFile "input.txt" --16964 wrong
    let blocks = tok [""] . lines $ filecontents

    -- Part 1
    print $ sum
          . map (\(u, v) -> u + v)
          . map (second (*100))
          . map ((&&&) findSymmCol findSymmRow)
          $ blocks
    
    -- Part 2

    print $ "---------- Done. ----------"
