-- The logic of this file is based on
-- https://github.com/ColonelPhantom/aoc2023/blob/main/Day12.hs.
-- I only performed some tiny edits for readibility,
-- and expanded upon some of the comments.
-- All credit goes to the original solution,
-- and all potential mistakes are solely due to me :).

import Utils ((&&&), tok)
import Data.Char (isDigit)
import Data.List (intercalate)
import Debug.Trace
import qualified Data.Array as A


-- Parse an input line, as specified in part 2.
parseLine :: String -> (String, [Int])
parseLine = (&&&) string constraints
  where
    string      = intercalate "?"
                . replicate 5
                . takeWhile (/=' ')
    constraints = concat
                . replicate 5
                . map read
                . tok ","
                . dropWhile (not . isDigit)


-- Count the number of valid solutions for a line.
countsValids :: [Char] -> [Int] -> Int
countsValids cs is = lookUp cs is
  where
    -- We make a 2d-array 'dp' of size i*j, where 'i' is the length of
    -- the input string, and 'j' the number of '#'-groups.
    -- The array contains the result, i.e. the number of valid solutions
    -- as a function of the number of chars dropped from the input string,
    -- and the number of dropped '#'-runs.
    (i, j) = (length cs, length is)
    dp     = A.array ((0,0), (i, j))
                     [((x,y), evaluate (drop x cs) (drop y is)) | x <- [0..i],
                                                                  y <- [0..j]]

    -- Look-up a state. Used in evaluate() to prevent duplicate calculations.
    lookUp dx dy = dp A.! (i - length dx, j - length dy)

    -- Evaluate a state, defined by the remaining characters and the remaining number of '#'-runs.
    -- Terminal solutions.
    evaluate [] []             = 1 -- Valid solution.
    evaluate [] _              = 0 -- Invalid solution, leftover '#'-runs
    evaluate ('#':_) []        = 0 -- Invalid solution, no more groups left.

    -- Intermediate solutions, to be solved recursively.

    -- On '.', we necessarily need to advance by one step.
    evaluate ('.':xs) ys       = lookUp xs ys

    -- On '#', we drop the head of [Int] as a damaged run.
    -- We check that a damaged run is indeed possible here,
    -- otherwise we return 0 as we cannot make it valid.
    -- Three conditions need to be fulfilled:
    -- (1) There have to be at least 'y' chars remaining to be y '#'.
    -- (2) In the next 'y', no '.' may occur (i.e. only '#' and '?')
    -- (3) After the 'y' chars, the input is either over, or the next
    --     char is not a '#', i.e. a '.' or a '?'.
    -- If these hold, we drop 'y'+1 chars (including the undamaged spring
    -- afterwards, and remove the current group from the [Int] list.
    evaluate xs@('#':_) (y:ys) = if   length xs >= y
                                        && '.' `notElem` (take y xs)
                                        && (length xs == y || xs !! y /= '#')
                                 then lookUp (drop (y+1) xs) ys
                                 else 0
    
    -- For ?, we first try the '.' case, i.e. just advance one cell.
    -- This case, we can safely look up in the 'dp' array using lookUp.
    -- We also try the '#' case, where we don't advance, but replace
    -- the '?' with an '#'.
    -- For the second case, we need to call evaluate() and not lookUp(),
    -- because the latter would yield an infinite loop, because the
    -- memoized version doesn't know we replaced the char).
    evaluate ('?':xs) ys = lookUp xs ys + evaluate ('#':xs) ys


-- Answers
main = do
  
    filecontents <- readFile "input.txt"

    -- Part 2
    print $ sum . map (uncurry countsValids) . map parseLine . lines $ filecontents
    

    print $ "---------- Done. ----------"
