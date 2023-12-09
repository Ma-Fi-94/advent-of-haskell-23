import Utils (readInteger, tok)

---------------
-- Functions --
---------------

-- Parse a line of blank-seperated integers
parseLine :: String -> [Integer]
parseLine = map readInteger . tok " "


-- Calculate iteratively the differences between subsequent numbers,
-- until we reach a list of all zeros. Returned list of lists includes
-- initial input, but not the final zeros, and is reversed for convenience.
diffsRev :: (Num a, Eq a) => [a] -> [[a]]
diffsRev = reverse . takeWhile (any (/=0)) . iterate diff
  where
    diff xs = zipWith (-) (tail xs) (init xs)


-- The actual prediction of the next element of a given list.
predictNext :: (Num a, Eq a) => [a] -> a
predictNext = go 0 . diffsRev 
  where
    go delta (l:[]) = (last l) + delta
    go delta (l:ls) = go delta' ls
      where
        delta' = (last l) + delta


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
