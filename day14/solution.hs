{-# Language NumericUnderscores #-}

import Data.List (transpose)
import Utils (firstRecElem)
import Data.Maybe (fromJust)


----------
-- Both --
----------


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


-- Calculate the load on the north support beams.
load :: [String] -> Int
load = sum
     . map (\(i, s) -> (*i) . length . filter (=='O') $ s)
     . zip [1..]
     . reverse


------------ 
-- Part 1 --
------------


tiltNorth :: [String] -> [String]
tiltNorth = transpose . map moveLeft . transpose


------------ 
-- Part 2 --
------------

-- A completet spin cycle.
spin :: [String] -> [String]
spin = tiltEast . tiltSouth . tiltWest . tiltNorth
  where
    tiltWest  = map moveLeft
    tiltEast  = map (reverse . moveLeft . reverse)
    tiltSouth = transpose . tiltEast . transpose


-- Answers
main = do
  
    filecontents <- readFile "input.txt"
    let grid = lines $ filecontents


    ------------
    -- Part 1 --
    ------------

    print $ load . tiltNorth $ grid

    ------------
    -- Part 2 --
    ------------
    
    -- We assume there's a cycle in it, so we
    -- naively look for the first recurring element.
    let steps    = iterate spin grid
    let (i1, i2) = fromJust . firstRecElem $ outs

    -- We assume the first recurring element denotes the cycle.
    -- We skip over all complete cycles and just have to look
    -- up the intermediate output at step i.
    let i             = i1 + (1_000_000_000 - i1) `rem` (i2 - i1)
    print (load (outs !! i))

    print $ "---------- Done. ----------"
