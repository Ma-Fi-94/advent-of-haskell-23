import Utils (tok, at, readInt)

-----------
-- Sugar --
-----------

type Race = (Int, Int)

-----------
-- Both --
-----------

-- Given allowed time and the length of the race,
-- we calculate the number of possible ways to win.
-- We use the fact that s(x) is an inverted parabola,
-- which implies there's only one region of valid solutions.
-- For extra performance, we could also use the quadratic formula,
-- to analytically find the two points of intersection.
nbSols :: Race -> Int
nbSols (t,d) = length 
             . takeWhile (>d) 
             . dropWhile (<=d)
             $ map s [0..t]
  where
    s x = (t-x) * x

------------
-- Part 1 --
------------

-- Parse the two input lines to a list of Race-s
parseInput1 :: [String] -> [Race]
parseInput1 xs = zip ts ds
  where
    ts        = parseLine (xs!!0)
    ds        = parseLine (xs!!1)
    parseLine = map readInt . tok " " . at 1 . tok ":"


------------
-- Part 2 --
------------

-- Parse the two input lines to a single Race
parseInput2 :: [String] -> Race
parseInput2 xs = (t, d)
  where
    t         = parseLine (xs!!0)
    d         = parseLine (xs!!1)
    parseLine = readInt . strip . at 1 . tok ":"
    strip [] = []
    strip (x:xs)
        |x == ' '  = strip xs
        |otherwise = x : strip xs
    

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
