import Utils (readInteger, tok, at, groupn)
import Data.List (sort)
import Debug.Trace (trace)

-- A ClosedInterval datatype plus some convenience functions
type ClosedInterval = (Integer, Integer)
(a, b) `containedIn` (c, d) = (a >= c) && (b <= d)
(a, b) `cutsInto` (c, _)    = (a < c) && (b >= c)


-- Sugar
type Rule           = (ClosedInterval, ClosedInterval)
type Table          = [Rule]


-- Parse a rule (line of three numbers) from the input text
parseRule :: String -> Rule
parseRule str = ((src, src+len-1), (dst, dst+len-1))
  where
    (dst, src, len) = (numbers !! 0, numbers !! 1, numbers !! 2)
    numbers         = map readInteger . tok " " $ str


-- Extract seed numbers from the first line of input text
getSeeds :: [String] -> [ClosedInterval]
getSeeds = map (\xs -> (xs!!0, (xs!!0) + (xs!!1) - 1))
         . groupn 2
         . map readInteger
         . tok " "
         . at 1
         . tok ":"
         . at 0


-- Apply a transformation rule to a ClosedInterval.
-- The interval needs to be entirely contained in the
-- source interval of the transformation rule.
apply :: Rule -> ClosedInterval -> ClosedInterval
apply r@((u, v), (w, _)) i@(a, b)
    |(a, b) `containedIn` (u, v) = (a + w - u, b + w - u)
    |otherwise                   = error "Interval not entirely contained."


-- Transform based on a Table a given ClosedInterval
-- This will produce one or more new ClosedInterval-s
transform :: Table -> ClosedInterval -> [ClosedInterval]
transform rules interval@(a,b)
    |any (interval `containedIn`) (map fst rules) = [apply containsRule interval]
    |any (`cutsInto` interval) (map fst rules)    = transform rules left1 ++ transform rules right1
    |any (interval `cutsInto`) (map fst rules)    = transform rules left2 ++ transform rules right2
    |otherwise                                    = [interval] 
  where
    containsRule = head . filter ((interval `containedIn`) . fst) $ rules

    cutsRule1    = head . filter ((`cutsInto` interval) . fst) $ rules
    left1        = (a, x1)
    right1       = (x1+1, b)
    x1           = snd $ fst cutsRule1

    cutsRule2    = head . filter ((interval `cutsInto`) . fst) $ rules
    left2        = (a, x2-1)
    right2       = (x2, b)
    x2           = fst $ fst cutsRule2

-------------
-- Answers --
-------------

main = do
    filecontents <- readFile "input.txt"

    let seeds = getSeeds . lines $ filecontents

    let tables = map (map parseRule)
               . map tail 
               . tok [""]
               . drop 2
               . lines $ filecontents

    print $ minimum
          . map fst
          . concat . map (transform (tables!!6)) 
          . concat . map (transform (tables!!5)) 
          . concat . map (transform (tables!!4)) 
          . concat . map (transform (tables!!3)) 
          . concat . map (transform (tables!!2)) 
          . concat . map (transform (tables!!1)) 
          . concat . map (transform (tables!!0)) 
          $ seeds


    print $ "---------- Done. ----------"
