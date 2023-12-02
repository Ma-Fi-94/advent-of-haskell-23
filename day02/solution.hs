import Utils (tok, at, endsWith, readInt)
import Data.Char (isDigit)
import Debug.Trace

-- Sugar
type GameID = Int
type RGB    = (Int, Int, Int)
type Game   = (GameID, RGB)

----------------
-- Both parts --
----------------

-- Parse an input line to a game, denoting maximum numbers of RGB dice each
parseLine :: String -> Game
parseLine line = (gameID, findMax (0,0,0) rgbTokens)
  where
    tokens    = tok ":,;" line
    gameID    = readInt . reverse . takeWhile isDigit . reverse . at 0 $ tokens
    rgbTokens = map (drop 1) $ drop 1 tokens

    findMax (r,g,b) [] = (r,g,b)
    findMax (r,g,b) (s:ss)
        |(endsWith "red" s)   = findMax (maximum [r, val], g, b) ss
        |(endsWith "green" s) = findMax (r, maximum [g, val], b) ss
        |(endsWith "blue" s)  = findMax (r, g, maximum [b, val]) ss
          where
            val = readInt . takeWhile isDigit $ s


------------
-- Part 1 --
------------

-- Only return those games where all values of RGB don't exceed given threshold
filterGamesThr :: RGB -> [Game] -> [Game]
filterGamesThr (rmax, gmax, bmax) games = filter allBelow games
  where
    allBelow = \(_, (r,g,b)) -> r <= rmax && g <= gmax && b <= bmax


------------
-- Part 2 --
------------

-- Calculate the "power" of a game, i.e. R*G*B
power :: Game -> Int
power (_, (r,g,b)) = r*g*b


-- The answer reading from external file
main = do
    filecontents <- readFile "input.txt"
    print $ sum . map fst . filterGamesThr (12,13,14) . map parseLine . lines $ filecontents
    print $ sum . map power . map parseLine . lines $ filecontents

    print $ "---------- Done. ----------"
