import Data.List (intersect)
import Utils (readInt, tok, at)

-----------
-- Sugar --
-----------

type Card  = ([Int], [Int]) -- (winning numbers, numbers on card)
type Stash = [(Int, Int)]   -- (number of matches, number of cards owned)

----------
-- Both --
----------

-- Parse a line of the input file to a Card.
parseLine :: String -> Card
parseLine s = (wins, numbers)
  where
    wins    = map readInt . tok " " $ left
    numbers = map readInt . tok " " $ right
    left    = tokens !! 1
    right   = tokens !! 2
    tokens  = tok ":|" s


-- Find the number of matching numbers on a Card.
nbMatches :: Card -> Int
nbMatches (wins, numbers) = length $ intersect wins numbers


------------
-- Part 1 --
------------


-- Calculate the score of a given card.
scoreCard :: Card -> Int
scoreCard (wins, numbers)
    |n == 1    = 1
    |n > 1     = 2 ^ (n - 1)
    |otherwise = 0
  where
    n = nbMatches (wins, numbers)
    

------------
-- Part 2 --
------------


-- Process the first card of a given stash of cards.
-- - We drop the first care
-- - The count of the next nbMatches cards are increased by nbOwned.
-- - The remaining cards are returned unchanged.
-- We don't have to worry about "overshooting", as Prelude.take
-- and Prelude.drop implicitly take care of lists that are too short.
step :: Stash -> Stash
step [] = []
step ((nbMatches, nbOwned):rest) = changeds ++ unchangeds
  where
    changeds   = map (\(m,o) -> (m,o+nbOwned)) (take nbMatches rest)
    unchangeds = drop nbMatches rest


-- The answer reading from external file
main = do
    filecontents <- readFile "input.txt"
    let cards = map parseLine . lines $ filecontents

    ------------
    -- Part 1 --
    ------------

    print $ sum . map scoreCard $ cards

    ------------
    -- Part 2 --
    ------------

    -- We start with one copy of every card in our stash
    let stash = zip (map nbMatches cards) (repeat 1)

    -- We step over the stash of cards from top to bottom
    -- Because step [] = [], we get an infinite list,
    -- so we only take non-empty lists.
    -- (I know this is kinda hacky and there prolly is a better way.)
    let steps = takeWhile (/=[]) $ iterate step stash

    -- Prettyprinting. This will produce a lot of output.
    -- mapM_ print steps

    -- The final answer is the sum of the nbOwned of all first elements
    print $ sum . map snd . map head $ steps

    print $ "---------- Done. ----------"
