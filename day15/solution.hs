import Utils (tok)
import Data.Char (ord)

----------
-- Both --
----------


------------ 
-- Part 1 --
------------

-- Calculate a string's hash
hash :: String -> Int
hash = go 0
  where
    go cur []     = cur
    go cur (c:cs) = go cur' cs
      where
        cur' = ((cur + (ord c)) * 17) `rem` 256

------------ 
-- Part 2 --
------------

main = do
  
    filecontents <- readFile "input.txt"
    let input = head $ lines $ filecontents


    ------------
    -- Part 1 --
    ------------

    print $ sum . map hash . tok "," $ input

    ------------
    -- Part 2 --
    ------------


    print $ "---------- Done. ----------"
