import Utils (tok, at, readDouble)

-----------
-- Sugar --
-----------

type Race = (Double, Double)

-----------
-- Both --
-----------

-- Given allowed time and the length of the race,
-- we calculate the number of possible ways to win.
-- For extra performance, we use the quadratic formula,
-- to directly find the two points of intersection.
nbSols :: Race -> Int
nbSols (t,d) = x1 - x2 + 1
  where
    -- We want at least (d+1) units of length here, not d.
    x1 = floor   $ (t/2) + sqrt (t^2/4 - (d+1))
    x2 = ceiling $ (t/2) - sqrt (t^2/4 - (d+1))

------------
-- Part 1 --
------------

-- Parse the two input lines to a list of Race-s
parseInput1 :: [String] -> [Race]
parseInput1 xs = zip ts ds
  where
    ts        = parseLine (xs!!0)
    ds        = parseLine (xs!!1)
    parseLine = map readDouble . tok " " . at 1 . tok ":"


------------
-- Part 2 --
------------

-- Parse the two input lines to a single Race
parseInput2 :: [String] -> Race
parseInput2 xs = (t, d)
  where
    t         = parseLine (xs!!0)
    d         = parseLine (xs!!1)
    parseLine = readDouble . filter (/=' ') . at 1 . tok ":"

-------------
-- Answers --
-------------

main = do
    filecontents <- readFile "input.txt"

    -- Part 1
    print $ product . map nbSols . parseInput1 . lines $ filecontents

    -- Part 2
    print $ nbSols . parseInput2 . lines $ filecontents


    print $ "---------- Done. ----------"
