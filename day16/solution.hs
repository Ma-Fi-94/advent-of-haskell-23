import Utils (tok)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Sugar.
-- For 'Grid' I could also use my custom Grid.hs, however instead we
-- use a Map, that allows accessing in O(1), instead of O(max(n,k)).
data Direction = N | S | W | E deriving (Show, Eq, Ord)
type Cell      = Char
type Coord     = (Int, Int)
type Grid      = (Int, Int, Map Coord Cell)


-- Parse input list of strings to a Grid
parseGrid :: [String] -> Grid
parseGrid ss = (h, w, m)
  where
    h   = length ss
    w   = length . head $ ss
    idx = (,) <$> [0..h-1] <*> [0..w-1]
    m   = Map.fromList . zip idx . concat $ ss


-- Given a starting position and direction, and a Grid,
-- count the number of visited fields.
-- The key insight here is to store in a Set all pairs of
-- (Coord, Direction), so we don't follow up on those,
-- which we have already visited.
count :: (Coord, Direction) -> Grid -> Int
count start grid = go (Set.singleton start) [start]
  where
    go visiteds heads
        -- this is more efficient than length . nub . map fst . Set.elems
        |heads == [] = Set.size . Set.map fst $ visiteds
        |otherwise   = go visiteds' heads'
          where
            -- all possible next pairs of (position, direction)
            nexts     = concatMap (step grid) heads
            -- the ones we need to still follow up on
            heads'    = filter (`Set.notMember` visiteds) nexts
            -- Add these to the set of visiteds 
            visiteds' = Set.union visiteds $ Set.fromList heads'
            


-- Given a Grid, the current position and the current direction,
-- take one step, yielding a list of tuples of position and direction.
step :: Grid -> (Coord, Direction) -> [(Coord, Direction)]
step (h, w, m) (pos, dir) = filter (valid . fst)
                          $ case (m Map.! pos) of
                                 '.'  -> empty pos dir
                                 '/'  -> mirrorF pos dir
                                 '\\' -> mirrorB pos dir
                                 '-'  -> splitterH pos dir
                                 '|'  -> splitterV pos dir
  where
    valid (i, j)     = i >= 0 && i < h && j >= 0 && j < w
    empty (i, j) N   = [((i - 1, j), N)]
    empty (i, j) S   = [((i + 1, j), S)]
    empty (i, j) W   = [((i, j - 1), W)]
    empty (i, j) E   = [((i, j + 1), E)]
    mirrorF (i, j) N = [((i, j + 1), E)]
    mirrorF (i, j) S = [((i, j - 1), W)]
    mirrorF (i, j) W = [((i + 1, j), S)]
    mirrorF (i, j) E = [((i - 1, j), N)]
    mirrorB (i, j) N = [((i, j - 1), W)]
    mirrorB (i, j) S = [((i, j + 1), E)]
    mirrorB (i, j) W = [((i - 1, j), N)]
    mirrorB (i, j) E = [((i + 1, j), S)]
    splitterH (i, j) N = [((i, j - 1), W), ((i, j + 1), E)]
    splitterH (i, j) S = [((i, j - 1), W), ((i, j + 1), E)]
    splitterH (i, j) W = [((i, j - 1), W)]
    splitterH (i, j) E = [((i, j + 1), E)]
    splitterV (i, j) N = [((i - 1, j), N)]
    splitterV (i, j) S = [((i + 1, j), S)]
    splitterV (i, j) W = [((i - 1, j), N), ((i + 1, j), S)]
    splitterV (i, j) E = [((i - 1, j), N), ((i + 1, j), S)]


------------ 
-- Part 2 --
------------

-- Bruteforce goes brrr
countAll :: Grid -> Int
countAll g@(h, w, m) = maximum
                     . map (\s -> count s g)
                     $ starts
  where
    starts = [((0, j), S)   | j <- [0..w-1]]
          ++ [((h-1, j), N) | j <- [0..w-1]]
          ++ [((i, 0), E)   | i <- [0..h-1]]
          ++ [((i, w-1), W) | i <- [0..h-1]]


main = do
  
    filecontents <- readFile "input.txt"
    let grid = parseGrid . lines $ filecontents


    ------------
    -- Part 1 --
    ------------

    print $ count ((0, 0), E) grid

    ------------
    -- Part 2 --
    ------------

    print $ countAll grid


    print $ "---------- Done. ----------"
