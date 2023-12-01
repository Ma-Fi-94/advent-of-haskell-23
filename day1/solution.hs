import Data.Char (isAlpha)

-- Part 1 is trivial
parseLine1 :: String -> Int
parseLine1 s = 10 * (firstDigit s) + (lastDigit s)
  where
    firstDigit = (read :: String -> Int) . take 1 . dropWhile isAlpha
    lastDigit  = firstDigit . reverse


-- Part 2:
parseLine2 :: String -> Int
parseLine2 s = 10 * (firstDigit s) + (lastDigit s)
  where
    firstDigit = parseF
    lastDigit  = parseR . reverse

-- Helper for easier comparison. First argument is the prefix to be checked
beginsWith :: String -> String -> Bool
beginsWith [] _          = True
beginsWith _ []          = False
beginsWith (x:xs) (y:ys) = (x == y) && beginsWith xs ys

-- Parsing a string forwards. Exceedingly ugly, my apologies.
parseF :: String -> Int
parseF []                 = error "lol no"
parseF s
    |beginsWith "one" s   = 1
    |beginsWith "two" s   = 2
    |beginsWith "three" s = 3
    |beginsWith "four" s  = 4
    |beginsWith "five" s  = 5
    |beginsWith "six" s   = 6
    |beginsWith "seven" s = 7
    |beginsWith "eight" s = 8
    |beginsWith "nine" s  = 9
    |beginsWith "1" s     = 1
    |beginsWith "2" s     = 2
    |beginsWith "3" s     = 3
    |beginsWith "4" s     = 4
    |beginsWith "5" s     = 5
    |beginsWith "6" s     = 6
    |beginsWith "7" s     = 7
    |beginsWith "8" s     = 8
    |beginsWith "9" s     = 9
parseF (_:xs)             = parseF xs

-- Parsing a reversed string forwards. As before.
parseR :: String -> Int
parseR []                  = error "lol no"
parseR s
    |beginsWith "eno" s   = 1
    |beginsWith "owt" s   = 2
    |beginsWith "eerht" s = 3
    |beginsWith "ruof" s  = 4
    |beginsWith "evif" s  = 5
    |beginsWith "xis" s   = 6
    |beginsWith "neves" s = 7
    |beginsWith "thgie" s = 8
    |beginsWith "enin" s  = 9
    |beginsWith "1" s     = 1
    |beginsWith "2" s     = 2
    |beginsWith "3" s     = 3
    |beginsWith "4" s     = 4
    |beginsWith "5" s     = 5
    |beginsWith "6" s     = 6
    |beginsWith "7" s     = 7
    |beginsWith "8" s     = 8
    |beginsWith "9" s     = 9
parseR (_:xs)             = parseR xs

-- The answer reading from external file
main = do
    filecontents <- readFile "input.txt"
    print $ sum . map parseLine1 . lines $ filecontents
    print $ sum . map parseLine2 . lines $ filecontents
    print $ "Done."
