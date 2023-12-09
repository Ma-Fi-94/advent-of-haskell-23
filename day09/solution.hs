import Utils (readInteger, tok)

---------------
-- Functions --
---------------

-- Parse a line of blank-seperated integers
parseLine :: String -> [Integer]
parseLine = map readInteger . tok " "


-- Calculate iteratively the differences between subsequent numbers,
-- until we reach a list of all zeros. Returned list of lists includes
-- initial input, but not the final zeros.
diffs :: (Num a, Eq a) => [a] -> [[a]]
diffs = takeWhile (any (/=0)) . iterate diff
  where
    diff xs = zipWith (-) (tail xs) (init xs)


-- The actual prediction of the next element of a given list.
-- We calculate the lists of differences, reverse it, and then
-- step through them, filling in and carrying over the respective last element.
predictNext :: (Num a, Eq a) => [a] -> a
predictNext = foldl step 0 . reverse . diffs 
  where
    step delta xs = delta + (last xs)


-------------
-- Answers --
-------------

main = do
    filecontents <- readFile "input.txt"
    let input = map parseLine . lines $ filecontents

    -- Part 1: Forward prediction
    print $ sum . map predictNext $ input

    -- Part 2: Just reverse all the input lists and predict "forwards" again.
    print $ sum . map predictNext . map reverse $ input

    print $ "---------- Done. ----------"
