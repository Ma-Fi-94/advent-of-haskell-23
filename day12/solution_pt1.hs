import Utils ((&&&), tok)
import Data.Char (isDigit)
import Data.List (group)


-- Parse an input line, as specified in part 1.
parseLine :: String -> (String, [Int])
parseLine = (&&&) string constraints
  where
    string      = takeWhile (/=' ')
    constraints = map read . tok "," . dropWhile (not . isDigit)


-- Validate a solution against the provided constraints
isValid :: [Int] -> [Char] -> Bool
isValid is cs = is == counts
  where
    counts = map length . filter ((=='#') . head) . group $ cs


-- Generate all possible solutions by replacing every '?'
-- with either '.' or '#'. This is very inefficient,
-- as it will grow O(2^k), where k is the count of '?'.
genSols :: String -> [String]
genSols [] = [[]]
genSols (c:cs)
    |c == '?'  = map ('.':) (genSols cs)
              ++ map ('#':) (genSols cs)
    |otherwise = map (c:) (genSols cs)


-- Given the input string and the constraints, counts the
-- number of valid solutions by generation all possible
-- combinations and filtering out these which don't fulfill
-- the constraints.
countValids :: ([Char], [Int]) -> Int
countValids (cs, is) = length . filter (isValid is) . genSols $ cs 

-- Answers
main = do
  
    filecontents <- readFile "input.txt"

    -- Part 1: Brute force. This takes ~2.2s on my system,
    -- so it will most certainly not scale well ;).
    print $ sum . map countValids . map parseLine . lines $ filecontents
    

    print $ "---------- Done. ----------"
