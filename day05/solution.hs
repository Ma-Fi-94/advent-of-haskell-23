import Utils (readInt, tok, at, groupn)
import Data.List (sort)

-----------
-- Sugar --
-----------

type Rule  = (Int, Int, Int) -- (src, dst, len) 
type Table = [Rule]          -- a block from the input text

----------
-- Both --
----------

-- Parse a line of three numbers from the input text
parseRule :: String -> Rule
parseRule str = (src, dst, len)
  where
    (src, dst, len) = (numbers !! 1, numbers !! 0, numbers !! 2)
    numbers         = map readInt . tok " " $ str

-- Look up the corresponding output to the input x in a Table of Rule-s
lookup' :: Table -> Int -> Int
lookup' [] x = x
lookup' ((src, dst, len):ms) x
    |(x >= src) && (x < src+len) = dst + x - src
    |otherwise                   = lookup' ms x

------------
-- Part 1 --
------------

-- Extract seed numbers from the first line of input text
getSeeds1 :: [String] -> [Int]
getSeeds1 input = map readInt . tok " " . at 1 . tok ":" . at 0 $ input

------------
-- Part 2 --
------------

-- Extract seed numbers from the first line of input text
getSeeds2 :: [String] -> [Int]
getSeeds2 input = concat . map expand $ sublists
  where
    sublists  = groupn 2 $ getSeeds1 input
    expand xs = [(xs!!0)..((xs!!0) + (xs!!1) - 1)] 

-------------
-- Answers --
-------------

main = do
    filecontents <- readFile "input.txt"

    -- Part 1
    let seeds = getSeeds1 . lines $ filecontents -- part 1
    -- let seeds = getSeeds2 . lines $ filecontents -- part 2, correct, but WAY too slow.

    let tables = map (map parseRule)             -- parse the rules for every table
               . map tail                        -- drop table headings
               . tok [""]                        -- separate tables
               . drop 2                          -- drop first two lines
               . lines $ filecontents
    
    let locations = map (lookup' (tables!!6))    -- Applying the transformation tables
                  . map (lookup' (tables!!5))
                  . map (lookup' (tables!!4))
                  . map (lookup' (tables!!3))
                  . map (lookup' (tables!!2))
                  . map (lookup' (tables!!1))
                  . map (lookup' (tables!!0))
                  $ seeds


    print $ minimum locations

    -- Part 2
    -- t.b.d. efficiently



    print $ "---------- Done. ----------"
