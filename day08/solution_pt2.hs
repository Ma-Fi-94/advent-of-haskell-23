import           Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

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


-- Extract all starting nodes, i.e. nodes with a name ending on A,
-- from the input file
getStarts :: [String] -> [String]
getStarts = filter (\s -> last s == 'A') . map (take 3) . drop 2


-- Given a map, a nodename and a direction, return the name of the next node.
step :: Map String (String, String) -> Direction -> String -> String
step nodemap d nodename = nodename'
  where
    nodename'
        |d == L  = fst (nodemap Map.! nodename)
        |d == R  = snd (nodemap Map.! nodename)


-- Count steps until we reach an end node , given a nodemap, a starting node, and
-- a list of directions to follow.
countSteps :: Map String (String, String) -> [Direction] -> String -> Int
countSteps nodemap (d:ds) nodename 
    |isEnd nodename = 0
    |otherwise      = 1 + countSteps nodemap ds nodename'
  where
    isEnd     = (=='Z') . last
    nodename' = step nodemap d nodename


-------------
-- Answers --
-------------

main = do
    filecontents <- readFile "input.txt"

    let directions = cycle . getDirections . lines $ filecontents
    let nodemap    = getMap . lines $ filecontents
    let starts     = getStarts . lines $ filecontents

    -- For every starting node, we calculate the number of steps needed
    -- to reach an end node.
    -- We assume that all paths are cyclic and only touch exactly one
    -- end node. Hence, we just have to find the LCM of the list of
    -- steps required for each node.
    print $ foldl1 lcm $ map (countSteps nodemap directions) starts

    print $ "---------- Done. ----------"
