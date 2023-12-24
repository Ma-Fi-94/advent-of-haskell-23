module Main where

import Math.LinearEquationSolver
import System.IO.Unsafe
import Data.Maybe (isJust, fromJust, catMaybes)
import Utils (tok)
import Debug.Trace (trace)
import System.Process

-- This script requires a working executable of Z3, to be placed
-- somewhere into $PATH.

-- Sugar
type Coord = (Integer, Integer, Integer)
type Entry = (Coord, Coord)


------------
-- Part 1 --
------------

-- Parse an input line
parseLine :: String -> Entry
parseLine l = ((x, y, z), (vx, vy, vz))
  where
    toks         = map read . tok ", @" $ l
    (x, y, z)    = (toks !! 0, toks !! 1, toks !! 2)
    (vx, vy, vz) = (toks !! 3, toks !! 4, toks !! 5)


-- Calculate intersection point, and return it iff it exists
-- and occurs at t > 0.
-- This takes a few minute, so for the impatient I have
-- added some sorf of progress meter :).
intersect2d :: Entry -> Entry -> Maybe (Rational, Rational)
intersect2d ((x1, y1, _), (vx1, vy1, _))
            ((x2, y2, _), (vx2, vy2, _))
                = trace ("Currently working on " ++ show (x1, y1))
                $ if   (isJust sol) && t1 > 0 && t2 > 0
                  then Just (x1', y1')
                  else Nothing
  where
   a   = [[vx1, -(vx2)], [vy1, -(vy2)]]
   b   = [x2 - x1, y2 - y1]
   sol = unsafePerformIO $ solveRationalLinearEqs Z3
                               (map (map fromIntegral) a)
                               (map fromIntegral b)
   t1  = (fromJust sol) !! 0
   t2  = (fromJust sol) !! 1
   x1' = (fromIntegral x1) + t1 * (fromIntegral vx1)
   y1' = (fromIntegral y1) + t1 * (fromIntegral vy1)


-- Given a list, return all unique pairs, i.e.
-- a1,a2 and a2,a1 are considered duplicates.
uniquePairs :: [a] -> [(a, a)]
uniquePairs []     = []
uniquePairs (x:xs) = [(x, y) | y <- xs] ++ uniquePairs xs


------------
-- Part 2 --
------------

scriptHeader :: [String]
scriptHeader = ["(declare-const sx Int)",
                "(declare-const sy Int)",
                "(declare-const sz Int)",
                "",
                "(declare-const svx Int)",
                "(declare-const svy Int)",
                "(declare-const svz Int)",
                "",
                "(declare-const t1 Int)",
                "(declare-const t2 Int)",
                "(declare-const t3 Int)",
                ""]


scriptFooter :: [String]
scriptFooter = ["(check-sat)",
                "(get-model)"]


scriptBlock :: (Int, Entry) -> [String]
scriptBlock (t, ((x0, y0, z0), (vx, vy, vz))) =
  ["(assert (= (+ sx (* t" ++ show t ++ " svx)) (+ " ++ show x0 ++ " (* t" ++ show t ++ " " ++ show vx ++ "))))",
   "(assert (= (+ sy (* t" ++ show t ++ " svy)) (+ " ++ show y0 ++ " (* t" ++ show t ++ " " ++ show vy ++ "))))",
   "(assert (= (+ sz (* t" ++ show t ++ " svz)) (+ " ++ show z0 ++ " (* t" ++ show t ++ " " ++ show vz ++ "))))",
   "" ]



main = do
  input       <- readFile "input.txt"
  let entries = map parseLine . lines $ input


  ------------
  -- Part 1 --
  ------------

  print $ length 
        . filter (\(x, y) -> x >= 200000000000000 
                          && x <= 400000000000000
                          && y >= 200000000000000
                          && y <= 400000000000000)
        . catMaybes
        . map (uncurry intersect2d)
        $ uniquePairs entries


  ------------
  -- Part 2 --
  ------------

  -- We now need to solve a system of nonlinear equations. We do this 
  -- manually in Z3, but generate the SMT script automatically.
  -- Note that we need to determine 6 variables for the initial conditions,
  -- and for every hailstone another one (the time of collision). However,
  -- every hailstone adds three equations (for x,y,z of collision) to the system,
  -- hence every hailstone reduces degrees of freedom by two. Thus, three
  -- hailstones provide enough information to make the system solvable.
  -- We assume that the input is constructed in a way that any 3 hailstones
  -- do the trick, so we just take the first three one of the input file.

  -- Make the script
  let script =  scriptHeader
             ++ (concatMap scriptBlock . zip [1..] $ take 3 entries)
             ++ scriptFooter
  writeFile "part2.smt" $ unlines script

  -- Run it through z3 by typing
  -- "z3 part2.smt" in the command line

  -- The output by Z3

  {-
  sat
  (
    (define-fun t3 () Int
      891640066892)
    (define-fun sz () Int
      177831791810924)
    (define-fun t2 () Int
      447383459952)
    (define-fun svz () Int
      179)
    (define-fun t1 () Int
      696407182343)
    (define-fun svy () Int
      210)
    (define-fun sx () Int
      187016878804004)
    (define-fun sy () Int
      175507140888229)
    (define-fun svx () Int
      192)
  )
  -}
  
  -- Thus, the answer is:
  print $ 187016878804004 + 175507140888229 + 177831791810924

  print $ "---------- Done. ----------"
