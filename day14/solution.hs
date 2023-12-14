{-# Language NumericUnderscores #-}

import Data.List (transpose)
import Data.Map qualified as Map

----------
-- Both --
----------



------------ 
-- Part 1 --
------------

-- Move all round rocks of a line to the left as much as possible.
moveLeft :: String -> String
moveLeft [] = []
moveLeft s@(c:cs)
    |c == 'O' = 'O' : moveLeft cs
    |c == '#' = '#' : moveLeft cs
    |c == '.' = (replicate nbO 'O') ++ (replicate nbDots '.') ++ moveLeft s'
      where
        nbO    = length . filter (=='O') . takeWhile (/='#') $ s
        nbDots = length . filter (=='.') . takeWhile (/='#') $ s
        s'     = dropWhile (/='#') s


tiltNorth :: [String] -> [String]
tiltNorth = transpose . map moveLeft . transpose


-- Calculate the load on the north support beams.
load :: [String] -> Int
load = sum
     . map (\(i, s) -> (*i) . length . filter (=='O') $ s)
     . zip [1..]
     . reverse

------------ 
-- Part 2 --
------------

-- A completet spin cycle.
spin :: [String] -> [String]
spin = tiltEast . tiltSouth . tiltWest . tiltNorth
  where
    tiltSouth = transpose . tiltEast . transpose
    tiltWest  = map moveLeft
    tiltEast  = map (reverse . moveLeft . reverse)


-- Idea: We try to find a cycle. This function returns
-- the first and second index of the first duplicate found.
-- Taken from https://github.com/glguy/advent/blob/main/solutions/src/2023/14.hs
findCycle :: Ord a => [a] -> (Int, Int)
findCycle xs = go Map.empty 0 xs
  where
    go _ _ []        = error "No cycle found after checking entire list."
    go seen i (x:xs) = case (Map.lookup x seen) of
                             Just j  -> (j,i)
                             Nothing -> go (Map.insert x i seen) (i+1) xs


-- Answers
main = do
  
    filecontents <- readFile "input.txt"
    let grid = lines $ filecontents


    -- Part 1
    print $ load . tiltNorth $ grid

    -- Part 2, assuming there's a cycle in it.
    let outs          = iterate spin grid
    let (start, next) = findCycle outs
    let i             = start + (1_000_000_000 - start) `rem` (next - start)
    print (load (outs !! i))

    print $ "---------- Done. ----------"
