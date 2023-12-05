import Utils (readInt, tok, at)
import Data.List (sort)

-----------
-- Sugar --
-----------

type Rule  = (Int, Int, Int) -- (src, dst, len) 
type Table = [Rule]          -- a block from the input text

------------
-- Part 1 --
------------

-- Parse a line of three numbers from the input text
parseRule1 :: String -> Rule
parseRule1 str = (src, dst, len)
  where
    (src, dst, len) = (numbers !! 1, numbers !! 0, numbers !! 2)
    numbers         = map readInt . tok " " $ str

-- Look up the corresponding output to the input x in a Table of Rule-s
lookup1 :: Table -> Int -> Int
lookup1 [] x = x
lookup1 ((src, dst, len):ms) x
    |(x >= src) && (x < src+len) = dst + x - src
    |otherwise                   = lookup1 ms x

-- Extract seed numbers from the first line of input text
getSeeds1 :: [String] -> [Int]
getSeeds1 input = map readInt . tok " " . at 1 . tok ":" . at 0 $ input

------------
-- Part 2 --
------------

-- Extract seed numbers from the first line of input text

-------------
-- Answers --
-------------

main = do
    filecontents <- readFile "input.txt"

    -- Part 1
    let seeds = getSeeds1 . lines $ filecontents -- part 1

    let tables = map (map parseRule1)            -- parse every tables' rules
               . map tail                        -- drop table headings
               . tok [""]                        -- separate tables
               . drop 2                          -- drop top two lines (seeds)
               . lines $ filecontents            -- split up file lines
    
    let locations = map (lookup1 (tables!!6))    -- Applying the transformation tables
                  . map (lookup1 (tables!!5))
                  . map (lookup1 (tables!!4))
                  . map (lookup1 (tables!!3))
                  . map (lookup1 (tables!!2))
                  . map (lookup1 (tables!!1))
                  . map (lookup1 (tables!!0))
                  $ seeds


    print $ minimum locations

    -- Part 2
    -- t.b.d. efficiently



    print $ "---------- Done. ----------"
