{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isAlpha)
import Data.Text (pack, unpack, replace)

-- Part 1 is trivial
parseLine :: String -> Int
parseLine s = 10 * (firstDigit s) + (lastDigit s)
  where
    firstDigit = (read :: String -> Int) . take 1 . dropWhile isAlpha
    lastDigit  = firstDigit . reverse


-- Part 2 will use the routine from Part 1 after this transformation
replaceWords :: String -> String
replaceWords = unpack
             . replace "nine"  "nine9nine"
             . replace "eight" "eigeht8eight"
             . replace "seven" "seven7seven"
             . replace "six"   "six6six"
             . replace "five"  "five5five"
             . replace "four"  "four4four"
             . replace "three" "three3three"
             . replace "two"   "two2two"
             . replace "one"   "one1one"
             . pack

-- The answer reading from external file
main = do
    filecontents <- readFile "input.txt"
    print $ sum . map parseLine . lines $ filecontents
    print $ sum . map (parseLine . replaceWords ) . lines $ filecontents
    print $ "Done."
