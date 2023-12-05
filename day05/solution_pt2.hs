import Utils (readInteger, tok, at, groupn)
import Data.List (sort)


-- A ClosedInterval datatype plus some convenience functions
type ClosedInterval  = (Integer, Integer)
(a, b) `isin` (c, d) = (a >= c) && (b <= d)
(a, b) `cuts` (c, _) = (a < c)  && (b >= c)


-- Sugar
type Rule  = (ClosedInterval, ClosedInterval)
type Table = [Rule]


-- Extract seed numbers from the first line of input text
getSeeds :: [String] -> [ClosedInterval]
getSeeds = map tuplify
         . groupn 2
         . map readInteger
         . tok " "
         . at 1
         . tok ":"
         . head
  where
    tuplify xs = (xs!!0, (xs!!0) + (xs!!1) - 1)


-- Extract tables from input text
getTables :: [String] -> [Table]
getTables = map (map parseRule)
          . map tail 
          . tok [""]
          . drop 2


-- Parse a rule (line of three numbers) from the input text
parseRule :: String -> Rule
parseRule str = (domain, image) 
  where
    domain  = (src, src + len - 1)
    image   = (dst, dst + len - 1)
    dst     = tokens !! 0
    src     = tokens !! 1
    len     = tokens !! 2
    tokens  = map readInteger . tok " " $ str


-- Apply a transformation rule to a ClosedInterval.
-- The interval needs to be entirely contained in the
-- domain of the transformation rule.
apply :: Rule -> ClosedInterval -> ClosedInterval
apply ((u, v), (w, _)) (a, b)
    |(a, b) `isin` (u, v) = (a + w - u, b + w - u)
    |otherwise            = error "Rule not applicable."


-- Transform, based on a table of Rule-s, a given ClosedInterval
-- This will produce one or more new ClosedInterval-s
transform :: Table -> ClosedInterval -> [ClosedInterval]
transform t (a,b)
    |rulesContain /= [] = [apply (head rulesContain) (a,b)]
    |rulesLeft    /= [] = transform t left1 ++ transform t right1
    |rulesRight   /= [] = transform t left2 ++ transform t right2
    |otherwise          = [(a,b)] 
  where
    -- Three possible cases:
    rulesContain = filter (((a,b) `isin`) . fst) $ t
    rulesLeft    = filter ((`cuts` (a,b)) . fst) $ t
    rulesRight   = filter (((a,b) `cuts`) . fst) $ t

    -- Case 0: Interval falls entirely into the domain of a rule.
    -- Nothing more to do.

    -- Case 1: A rule cuts the interval from the left
    left1        = (a, cut1)
    right1       = (cut1+1, b)
    cut1         = snd . fst . head $ rulesLeft

    -- Case 2: A rule cuts the interval from the right
    left2        = (a, cut2-1)
    right2       = (cut2, b)
    cut2         = fst . fst . head $ rulesRight


-------------
-- Answers --
-------------

main = do
    filecontents <- readFile "input.txt"
    let seeds = getSeeds . lines $ filecontents
    let tables = getTables . lines $ filecontents

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
