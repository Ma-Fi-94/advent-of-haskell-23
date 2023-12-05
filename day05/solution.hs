import Utils (readInt, tok, at)
import Data.List (sort)

-- Sugar
type Rule  = (Int, Int, Int) -- (src, dst, len) 
type Table = [Rule]          -- a block from the input text

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

-- Extract seed numbers from the first line of input text
getSeeds :: [String] -> [Int]
getSeeds input = map readInt . tok " " . at 1 . tok ":" . at 0 $ input

-------------
-- Answers --
-------------

main = do
    filecontents <- readFile "input.txt"

    -- Part 1
    let seeds = getSeeds . lines $ filecontents -- part 1

    let tables = map (map parseRule)            -- parse every tables' rules
               . map tail                        -- drop table headings
               . tok [""]                        -- separate tables
               . drop 2                          -- drop top two lines (seeds)
               . lines $ filecontents            -- split up file lines
    
    let locations = map (lookup' (tables!!6))    -- Applying the transformation tables
                  . map (lookup' (tables!!5))
                  . map (lookup' (tables!!4))
                  . map (lookup' (tables!!3))
                  . map (lookup' (tables!!2))
                  . map (lookup' (tables!!1))
                  . map (lookup' (tables!!0))
                  $ seeds


    print $ minimum locations


    print $ "---------- Done. ----------"
