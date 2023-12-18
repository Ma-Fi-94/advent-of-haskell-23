import Utils (tok)

-- Sugar
data Direction = U| D | L | R deriving (Eq, Show)
type Count     = Int
type Colour    = String
type Entry     = (Direction, Count)
type Coord     = (Int, Int)

------------ 
-- Part 1 --
------------


-- Parse an input line to a tuple (direction, count, colour).
parseLine :: String -> Entry
parseLine l = (dir, cnt)
  where
    tokens = tok " " l
    dir    = case (tokens!!0) of {"U" -> U; "D" -> D; "L" -> L; "R" -> R}
    cnt    = read $ tokens!!1


-- Given a starting point and a list of steps,
-- returning the vertices of the trench.
path :: Coord -> [Entry] -> [Coord]
path pos es = scanl step pos es
  where
    step (r, c) (U, i) = (r - i, c)
    step (r, c) (D, i) = (r + i, c)
    step (r, c) (L, i) = (r, c - i)
    step (r, c) (R, i) = (r, c + i)


-- Apply the Trapezoid rule to a list of coordinates,
-- to get the area of the polygon.
-- We do need to use `abs` as the sum might be negative.
-- We need the first coordinate appear at the end of the list.
area :: [Coord] -> Int
area cs = abs . (`div` 2) . sumOfProducts $ cs
  where
    sumOfProducts (_:[])                  = 0
    sumOfProducts ((y1,x1):c2@(y2,x2):cs) = p + sumOfProducts (c2:cs)
      where
        p = (y1 + y2) * (x1 - x2)

------------ 
-- Part 2 --
------------


main = do
  
    filecontents <- readFile "input.txt"
    let steps = map parseLine . lines $ filecontents
    print $ steps
    print $ path (0,0) steps

    ------------
    -- Part 1 --
    ------------

    -- Inner area via Shoelace Formula again, see day 10.
    let boundary = path (0, 0) steps
    let a = area $ boundary

    -- The border now has a thickness of 1. Hence, we just
    -- add all lengths of the steps.
    let b = sum . map snd $ steps

    -- Pick's Theorem, once again!
    print $ a + (b `div` 2) + 1



    ------------
    -- Part 2 --
    ------------



    print $ "---------- Done. ----------"
