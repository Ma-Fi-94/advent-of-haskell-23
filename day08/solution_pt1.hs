import           Data.Map (Map)
import qualified Data.Map as Map

-----------
-- Sugar --
-----------

data Direction = L | R deriving (Show, Eq)
type Node = (String, (String, String))


---------------
-- Functions --
---------------

-- Extract the directions from the input file
getDirections :: [String] -> [Direction]
getDirections = go . head
  where
    go []       = []
    go ('L':xs) = L : go xs
    go ('R':xs) = R : go xs


-- Extract the node entries from the input file
getMap :: [String] -> Map String (String, String)
getMap = Map.fromList . map parseLine . drop 2
  where
    -- Parse a single entry
    parseLine cs = (name, (left, right))
      where
        name  = take 3 cs
        left  = take 3 . drop 7 $ cs
        right = take 3 . drop 12 $ cs


-- Given a map, a nodename and a direction, return the name of the next node.
step :: Map String (String, String) -> String -> Direction -> String
step nodemap nodename dir = nodename'
  where
    nodename'
        |dir == L  = fst (nodemap Map.! nodename)
        |dir == R  = snd (nodemap Map.! nodename)
        |otherwise = error "Invalid direction."


-- Count steps until we reach node "ZZZ", given a nodemap, a starting node, and
-- a list of directions to follow.
countSteps :: Map String (String, String) -> String -> [Direction] -> Int
countSteps _ "ZZZ" _               = 0
countSteps nodemap nodename (d:ds) = 1 + countSteps nodemap nodename' ds
  where
    nodename' = step nodemap nodename d 


-------------
-- Answers --
-------------

main = do
    filecontents <- readFile "input.txt"

    let directions = cycle . getDirections . lines $ filecontents
    let nodemap    = getMap . lines $ filecontents
    print $ countSteps nodemap "AAA" directions 


    print $ "---------- Done. ----------"
