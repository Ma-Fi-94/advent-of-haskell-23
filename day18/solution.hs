import Control.Arrow (first, second)
import Data.Char (digitToInt)
import Numeric (readHex)
import Utils (tok)


---------- 
-- Both --
----------


-- Sugar
data Direction = R | D | L | U deriving (Eq, Show, Enum)
type Count     = Int
type Colour    = String
type Entry     = (Direction, Count)
type Coord     = (Int, Int)


-- Given a starting point and a list of steps,
-- returning the vertices of the trench.
path :: Coord -> [Entry] -> [Coord]
path pos es = scanl step pos es
  where
    step p (D, i) = first  (+i) p
    step p (R, i) = second (+i) p
    step p (L, i) = second (-i+) p
    step p (U, i) = first  (-i+) p


-- Apply the Trapezoid rule to a list of coordinates,
-- to get the area of the polygon.
-- We do need to use `abs` as the sum might be negative.
-- We need the first coordinate appear at the end of the list.
area :: [Coord] -> Int
area cs = abs . (`div` 2) . sumOfProducts $ cs
  where
    sumOfProducts (_:[])                        = 0
    sumOfProducts ((y1, x1) : c2@(y2, x2) : cs) = p + sumOfProducts (c2:cs)
      where
        p = (y1 + y2) * (x1 - x2)


-- Solve for the complete area including the trenches, using Pick's Theorem
solve :: [Entry] -> Int
solve steps = a + (b `div` 2) + 1
  where
    -- Inner area via Shoelace Formula, see day 10.
    boundary = path (0, 0) steps
    a        = area $ boundary

    -- Length of the boundary is the sum of the length of all steps.
    b = sum . map snd $ steps


------------ 
-- Part 1 --
------------


-- Parse an input line to a tuple (direction, count).
parseLine :: String -> Entry
parseLine l = (dir, cnt)
  where
    tokens = tok " " l
    dir    = case (tokens!!0) of {"U" -> U; "D" -> D; "L" -> L; "R" -> R}
    cnt    = read $ tokens!!1


------------ 
-- Part 2 --
------------


-- Parse an input line to a tuple (direction, count).
parseLine2 :: String -> Entry
parseLine2 l = (dir, cnt)
  where
    hex = init . drop 2 . last . tok " " $ l
    dir = toEnum . digitToInt . last $ hex
    cnt = fst . head . readHex . init $ hex


main = do
  
    filecontents <- readFile "input.txt"

    -- Part 1
    print $ solve . map parseLine . lines $ filecontents

    -- Part 2
    print $ solve . map parseLine2 . lines $ filecontents

    print $ "---------- Done. ----------"
