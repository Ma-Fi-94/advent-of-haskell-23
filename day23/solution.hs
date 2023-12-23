import Data.List (elemIndex, (\\))
import Data.Maybe (fromJust)
import Grid

-----------
-- Sugar --
-----------

type Maze = Grid Char


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
neighbours :: Maze -> Coord -> [Coord]
neighbours m (r, c) = case (cell m (r, c)) of
                      '>' -> [(r, c + 1)]
                      '<' -> [(r, c - 1)]
                      '^' -> [(r - 1, c)]
                      'v' -> [(r + 1, c)]
                      '.' -> map fst
                           . filter ((/='#') . snd)
                           $ vonNeum m (r, c)


-- The actual search: Returns a list of all path length from start to finish.
dfs :: Maze -> Coord -> [Int]
dfs m c = go 0 [] [c]
  where
    go ctr visited queue
        |queue == []   = [] 
        |q == finish m = [ctr]                           ++ go ctr visited qs
        |otherwise     = go (ctr + 1) (q:visited) neighs ++ go ctr visited qs
          where
            (q:qs) = queue
            neighs = (neighbours m q) \\ visited

main = do

    ------------
    -- Part 1 --
    ------------

    input    <- readFile "test.txt"
    let maze =  Grid.fromList . lines $ input

    print $ maximum $ dfs maze (start maze)

    print $ "---------- Done. ----------"



