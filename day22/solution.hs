{-# LANGUAGE LambdaCase #-}

import Data.List (sortOn, intersect, (\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Utils (tok, fst3, snd3, thi3)


-----------
-- Sugar --
-----------

type Coord3    = (Int, Int, Int)   -- (x,y,z)
type Block     = (Coord3, Coord3)

type HeightMap = Map Coord2 Height
type Coord2    = (Int, Int)        -- (x,y)
type Height    = Int


-- Parses an input line, assuming it is wellformed.
parseLine :: String -> Block
parseLine s = ((x1, y1, z1), (x2, y2, z2))
  where
    toks         = map read . tok ",~" $ s
    (x1, y1, z1) = (toks !! 0, toks !! 1, toks !! 2)
    (x2, y2, z2) = (toks !! 3, toks !! 4, toks !! 5)


-- Sort blocks by their minimum z coordinate, assuming
-- the minimum is in the first of the two tuples.
sortBlocks :: [Block] -> [Block]
sortBlocks = sortOn (thi3 . fst)


-- Extract all 2d cells occupied by a block, assuming
-- x1 <= x2, and y1 <= y2.
shadow :: Block -> [Coord2]
shadow ((x1, y1, _), (x2, y2, _)) = [(x, y) | x <- [x1..x2],
                                              y <- [y1..y2]]


-- Given a HeightMap, let a given Block fall to the bottom,
-- return updated HeightMap and updated Block.
fall :: HeightMap -> Block -> (HeightMap, Block)
fall hm b@((x1, y1, z1), (x2, y2, z2)) = (hm', b')
  where
    b'  = ((x1, y1, z1'), (x2, y2, z2'))
    z1' = (+1) . maximum . map (hm Map.!) . shadow $ b
    z2' = z1' + z2 - z1
    hm' = Map.union (Map.fromList (zip (shadow b) (repeat z2'))) hm


-- Given a HeightMap, let all given Block-s fall to the bottom,
-- return updates HeightMap and updated list of Block-s.
fallAll :: HeightMap -> [Block] -> [Block]
fallAll hm bs = go hm bs []
  where
    go :: HeightMap -> [Block] -> [Block] -> [Block]
    go hm []     fallens = fallens
    go hm (b:bs) fallens = go hm' bs (fallens ++ [b'])
      where
        (hm', b') = fall hm b


-- Given a list of Block-s, and one Block, return all Block-s
-- which are placed directly on top of the given Block.
-- Nicely enough, the list can safely include the given single block,
-- as a block cannot be place directly on top of itself ;).
aboves :: [Block] -> Block -> [Block]
aboves bs b@(_, (_, _, z2)) = bs'
  where
    bs' = filter (\x -> (shadow x `intersect` shadow b) /= [])   -- whose shadow overlaps the block's shadow
        . filter (\((_, _, zmin), _) -> zmin == z2 + 1)          -- only blocks from row directly above
        $ bs


-- Given a list of Block-s, and one Block, return all Block-s
-- which are placed directly below the given Block.
-- Nicely enough, the list can safely include the given single block,
-- as a block cannot be place directly on top of itself ;).
belows :: [Block] -> Block -> [Block]
belows bs b@((_, _, z1), _) = bs'
  where
    bs' = filter (\x -> (shadow x `intersect` shadow b) /= [])   -- whose shadow overlaps the block's shadow
        . filter (\(_, (_, _, zmax)) -> zmax == z1 - 1)          -- only blocks from row directly below
        $ bs


-- Test whether a Block is removable, given the list of all Block-s.
-- This is the case, iff all of the Block-s directly above the given
-- Block-s are placed on at least another block
removable :: [Block] -> Block -> Bool
removable bs b = all (\x -> ((belows bs x) \\ [b]) /= []) blocksAbove
  where
    blocksAbove = aboves bs b


-- 488 too low


main = do

    ------------
    -- Part 1 --
    ------------

    -- Read and sort blocks by minimum z coordinate
    input <- sortBlocks . map parseLine . lines <$> readFile "input.txt"

    -- Make a map that tracks maximum occupied height for every (x,y).
    -- Initialise with the ground at z=0.
    let heightMap = Map.fromList [((x, y), 0) | x <- [0..500],
                                                y <- [0..500]]

    -- Let blocks fall down
    let blocks' = fallAll heightMap $ input
  
    -- Count how many can be removed.
    print $ length . filter (removable blocks') $ blocks'


    print $ "---------- Done. ----------"


