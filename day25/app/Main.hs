module Main where

import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Utils (tok, (&&&))


-- Sugar
type Edge  = (String, String)
type Node  = String
type Graph = Map Node (Set Node)


-- Construct *all* edges from input file.
-- This takes into account that in the input
-- edges may only be present once instead of
-- bidirectionally.
parseInput :: String -> [Edge]
parseInput = nub . concatMap parseLine . lines
  where
    parseLine l = [(head toks, t) | t <- tail toks]
               ++ [(t, head toks) | t <- tail toks]
      where
        toks = tok ": " l


-- Parse input file to list of connections
makeGraph :: [Edge] -> Graph
makeGraph = go Map.empty
  where
    go g []            = g
    go g ((n1, n2):es) = go g' es
      where
        g' = if  n1 `Map.member` g
                 then Map.adjust (Set.insert n2) n1 g
                 else Map.insert n1 (Set.singleton n2) g


-- Cut the connection between two nodes
cut :: Graph -> (Node, Node) -> Graph
cut g (n1, n2) = Map.adjust (Set.delete n2) n1
               . Map.adjust (Set.delete n1) n2
               $ g


-- Get a list of neighbours nodes of a given node
neighbours :: Graph -> Node -> [Node]
neighbours g n = Set.elems (g Map.! n)


-- Find the size of a connected region of a graph,
-- given a starting node.
sizeRegion :: Graph -> Node -> Int
sizeRegion graph node = go [] [node]
  where
    go visited []    = length visited
    go visited queue = go visited' queue'
      where
        visited' = nub $ visited ++ queue
        queue'   = filter (`notElem` visited)
                 $ concatMap (neighbours graph) queue 


main :: IO ()
main = do
    -- Construct a graph from the input
    input     <- readFile "input.txt"
    let graph = makeGraph . parseInput $ input


    -- I produced a "spring layout" graph of my input, which revealed
    -- the following three edges to cut:
    let graph' = foldl cut graph [("sss", "pzr"),
                                  ("zvk", "sxx"),
                                  ("pbx", "njx")]


    -- Now we just need to find the size of the two connected regions,
    -- and return their product.
    let size1 = sizeRegion graph' "sss"
    let size2 = sizeRegion graph' "pzr"
    print $ size1 * size2


    print $ "---------- Done. ----------"
