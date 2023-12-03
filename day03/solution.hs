import Data.Char (isDigit)
import Utils (readInt)

-- Some sugar
type Board     = [String]
type Coord     = (Int, Int)
type Symbol    = Coord
type Number    = (Coord, Int)
type Numberbox = (Coord, Coord)

type Star      = Symbol

----------
-- Both --
----------

-- Extract all numbers and their positions from a board
getNumbers :: Board -> [Number]
getNumbers = go 0 0 
  where
    go _ _ []         = []
    go r _ ([]:lists) = go (r+1) 0 lists
    go r c (list@(x:xs):lists)
        |(not . isDigit) x = go r (c+1) (xs:lists)
        |otherwise         = ((r,c), number) : go r c' (xs':lists)
          where
            number = readInt . takeWhile isDigit $ list
            offset = length . show $ number
            xs'    = drop offset list
            c'     = c + offset


-- Find the box around a number.
-- I.e. the upperleft and lowerright coordinates directly next to the number.
box :: Number -> Numberbox
box ((r,c), number) = ((r-1, c-1), (r+1, c2))
  where
    c2 = c + (length . show $ number)

------------
-- Part 1 --
------------

-- Extract the position of all symbols from a board
getSymbols :: Board -> [Symbol]
getSymbols = go 0 0
  where
    go _ _ []             = []
    go r _ ([]:lists)     = go (r+1) 0 lists
    go r c ((x:xs):lists)
        |(isDigit x) || (x == '.') = go r (c+1) (xs:lists)
        |otherwise                 = (r, c) : go r (c+1) (xs:lists)


-- Check whether, given the position of all symbols, a given number is a "part number".
-- I.e.: check whether any of the symbols is adjacent to the given number
isPartNb :: [Symbol] -> Number -> Bool
isPartNb symbols number = any (within numberbox) symbols
  where
    numberbox                         = box number
    within ((r1, c1), (r2, c2)) (r,c) = (r >= r1) && (r <= r2)
                                      && (c >= c1) && (c <= c2)

------------
-- Part 2 --
------------

-- Extract the position of all stars from a board
getStars :: Board -> [Star]
getStars = go 0 0
  where
    go _ _ []             = []
    go r _ ([]:lists)     = go (r+1) 0 lists
    go r c ((x:xs):lists)
        |x == '*'  = (r, c) : go r (c+1) (xs:lists)
        |otherwise = go r (c+1) (xs:lists)


-- Check whether a number is adjacent to a given star
numberAdjToStar :: Star -> Number -> Bool
numberAdjToStar (r,c) number = (r >= r1) && (r <= r2)
                             && (c >= c1) && (c <= c2)
  where
    ((r1, c1), (r2, c2)) = box number


-- Given all numbers, calculate the gear ratio of a given star
-- If there are two numbers adjacent to the star, return their product.
-- Else return zero, for convenience.
gearRatio :: [Number] -> Star -> Int
gearRatio numbers star = case (length adjacentNumbers) of
                         2 -> product $ map snd $ adjacentNumbers
                         _ -> 0
  where
    adjacentNumbers = filter (numberAdjToStar star) numbers 


-- The answer reading from external file
main = do
    filecontents <- readFile "input.txt"

    -- Part 1
    let symbols = getSymbols . lines $ filecontents
    let numbers = getNumbers . lines $ filecontents
    print $ sum . map snd . filter (isPartNb symbols) $ numbers

    -- Part 2
    let stars = getStars . lines $ filecontents
    print $ sum . map (gearRatio numbers) $ stars

    print $ "---------- Done. ----------"
