import Control.Arrow (second)
import Data.List (elemIndex, (\\))
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Grid
import Utils (cart)

import Debug.Trace (trace)

-----------
-- Sugar --
-----------

type Maze    = Grid Char
type DistMap = Map (Coord, Coord) Int

-- Get start point in the first row
start :: Maze -> Coord
start m@(Grid r c _) = (0, c')
  where
    c' = fromJust . elemIndex '.' $ row m 0


-- Get finish point in the last row
finish :: Maze -> Coord
finish m@(Grid r c _) = (r - 1, c')
  where
    c' = fromJust . elemIndex '.' $ row m (r - 1)


-- Given the Maze and a Coordinate, find all possible next Coordinates.
-- Now we ignore the slopes, as they're walkable bidirectionally.
neighbours :: Maze -> Coord -> [Coord]
neighbours m (r, c) = map fst
                    . filter ((/='#') . snd)
                    $ vonNeum m (r, c)


-- Returns a list of all path length from the first to the second coordinate.
-- We also provide a list of forbidden coordinates that may not be touched.
-- We simply treat these as already visited internall.
dfs :: Maze -> Coord -> Coord -> [Coord] -> [Int]
dfs m start finish forbidden = go 0 (Set.fromList forbidden) [start]
  where
    go ctr visited queue
        |queue == [] = [] 
        |q == finish = [ctr]                      ++ go ctr visited qs
        |otherwise   = go (ctr+1) visited' neighs ++ go ctr visited qs
          where
            (q:qs)   = queue
            neighs   = filter (`Set.notMember` visited) (neighbours m q)
            visited' = (Set.insert q visited)


-- Another dfs, which does not operate on the Maze anymore, but on a simple
-- Map (Coord, Coord) -> Int
dfs' :: DistMap -> Coord -> Coord -> [Int]
dfs' distmap start finish = go Set.empty [(start, 0)]
  where
    go :: Set Coord -> [(Coord, Int)] -> [Int]
    go visited queue
        |queue == []        = []
        -- For the impatient, print out promising intermediate results:
        |curCoord == finish && curDist > 6490 = trace (show curDist) $ [curDist]          ++ go visited qs        
        |curCoord == finish = [curDist]          ++ go visited qs
        |otherwise          = go visited' queue' ++ go visited qs
          where
            (q:qs)   = queue
            curCoord = fst q
            curDist  = snd q
            queue'   = filter ((`Set.notMember` visited) . fst)
                     . map (\((p1, p2), d) -> (p2, d + curDist))
                     . Map.assocs
                     . Map.filterWithKey (\(p1, p2) d -> p1 == (fst q))
                     $ distmap
            visited' = Set.insert curCoord visited



main = do

    input    <- readFile "input.txt"
    let maze = Grid.fromList . lines $ input

    -- First, we need to find all waypoints, i.e. walkable cells that have
    -- more than exactly two walkable neighbours.
    let waypoints = map fst 
                  . filter ((> 2) . length . snd)
                  . map (\(pos, content) -> (pos, neighbours maze pos))
                  . filter ((/= '#') . snd)
                  $ enumerate maze

    -- We only care about these waypoints, as well as the start and finish.
    let points = [start maze, finish maze] ++ waypoints

    -- Now we calculate the longest distance between every pair of
    -- different 'points', that is adjacent (= there's no other 'point'
    -- inbetween).
    let pairs    = filter (\(p1, p2) -> p1 /= p2) 
                 $ cart points points
    let distList = map (\(p1, p2) -> dfs maze p1 p2 (points \\ [p1, p2])) pairs

    -- From this, we construct our new graph. For any pair of nodes (a, b),
    -- if there is an empty list of distances between them, a and b are
    -- not connected. If the list of distances is non-empty, we take
    -- its maximum as edge weight.
    let distMap = Map.fromList
                . map (second maximum)
                . filter ((/= []) . snd)
                $ zip pairs distList

    -- And the actual search.
    -- This takes a bit of time, but promising intermediate results (> 6490)
    -- are printed in real-time. So while waiting, you can already try some
    -- of these ;3.
    print $ maximum $ dfs' distMap (start maze) (finish maze)

    print $ "---------- Done. ----------"



