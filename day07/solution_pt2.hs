import Utils (tok, at, readInt)
import Data.List (group, sort, sortBy)

-----------
-- Sugar --
-----------

data Card = J | N2 | N3 | N4 | N5
          | N6 | N7 | N8 | N9 | N10
          | Q | K | A 
  deriving (Show, Enum, Eq, Ord)

data Type = HC | P | PP | SAME3 | FH | SAME4 | SAME5
  deriving (Show, Enum, Eq, Ord)

type Hand = [Card]
type Bid  = Int

---------------
-- Functions --
---------------

-- Parse an input line
parseLine :: String -> (Hand, Bid)
parseLine s = (h, b)
  where
    h = map char2Card . at 0 . tok " " $ s
    b = readInt . at 1 . tok " " $ s
    char2Card c = case c of { '2' -> N2; '3' -> N3; '4' -> N4;
                              '5' -> N5; '6' -> N6; '7' -> N7;
                              '8' -> N8; '9' -> N9; 'T' -> N10;
                              'J' -> J; 'Q' -> Q; 'K' -> K;
                              'A' -> A }


-- Find the type of a hand, also accounting for jokers.
handType :: Hand -> Type
handType h
    |freq0 + nbJok == 5         = SAME5
    |freq0 + nbJok >= 4         = SAME4
    |freq0 + freq1 + nbJok >= 5 = FH
    |freq0 + nbJok >= 3         = SAME3
    |freq0 + freq1 + nbJok >= 4 = PP
    |freq0 + nbJok >= 2         = P
    |otherwise                  = HC
  where
    nbJok      = length . filter (== J) $ h
    freqsNoJok = reverse . sort . map length . group . sort . filter (/= J) $ h
    freq0      = if (freqsNoJok /= [])      then (freqsNoJok!!0) else 0
    freq1      = if (length freqsNoJok > 1) then (freqsNoJok!!1) else 0



-- Compare two hands. First by handType, alternatively left-to-right
compHands :: Hand -> Hand -> Ordering
compHands h1 h2
   |compHandTypes h1 h2 /= EQ = compHandTypes h1 h2
   |otherwise                 = compLTR h1 h2
  where
    compHandTypes x y = compare (handType x) (handType y)

    compLTR [] [] = EQ
    compLTR (c:cs) (d:ds)
        |c /= d    = compare c d
        |otherwise = compLTR cs ds

-------------
-- Answers --
-------------

main = do
    filecontents <- readFile "input.txt"
    
    let input = map parseLine . lines $ filecontents

    print $ sum
          . zipWith (*) [1..]
          . map snd
          . sortBy (\x y -> compHands (fst x) (fst y))
          $ input


    print $ "---------- Done. ----------"
