import Utils (tok, readInt)
import Data.Char (ord, isDigit)
import Debug.Trace (trace)

----------
-- Both --
----------


------------ 
-- Part 1 --
------------

-- Calculate a string's hash
hash :: String -> Int
hash = go 0
  where
    go cur []     = cur
    go cur (c:cs) = go cur' cs
      where
        cur' = ((cur + (ord c)) * 17) `rem` 256

------------ 
-- Part 2 --
------------

-- Sugar. Give me sugar.
type LensLabel     = String
type BoxNb           = Int

type Lens          = (LensLensLabel, FocLength)
type LensLensLabel = String
type FocLength     = Int

data Command       = Remove CmdRemove | Add CmdAdd deriving Show
type CmdRemove     = (BoxNb, LensLabel)
type CmdAdd        = (BoxNb, LensLabel, FocLength)

type Boxchain      = [[Lens]]


-- Parse an input string to a command.
parseCommand :: String -> Command
parseCommand s
    |'-' `elem` s = Remove (box, label)
    |'=' `elem` s = Add    (box, label, focLen)
    |otherwise    = error "solution.parseCommand: Parse failed."
      where
        label  = takeWhile (not . (`elem` "=-")) s
        box    = hash label
        focLen = readInt $ dropWhile (not . isDigit) $ s


-- Try to remove a lens, given by its label, from a specific box.
-- If the label is not present in the box, nothing happens.
removeLens :: Boxchain -> BoxNb -> LensLabel -> Boxchain
removeLens boxes i lab = (take i boxes) ++ [box'] ++ (drop (i+1) boxes)
  where
    box' = filter ((/=lab) . fst) (boxes !! i)


-- Add a lens with a given label to the specified box of the boxchain.
-- If the label is new, the lens is added at the end of the list.
-- However, if there already is a lens with the same label in the box,
-- we update its focal lenght.
addLens :: Boxchain -> BoxNb -> Lens -> Boxchain
addLens boxes i (lab, foc) = (take i boxes) ++ [box'] ++ (drop (i+1) boxes)
  where
    box   = boxes!!i
    isNew = lab `elem` (map fst box) 
    box'  = if   not isNew
            then box  ++ [(lab, foc)]
            else left ++ [(lab, foc)] ++ right
      where
        left  = takeWhile ((/=lab) . fst) box
        right = drop 1 . dropWhile ((/=lab) . fst) $ box


-- Dispatch command execution
execCommand :: Boxchain -> Command -> Boxchain
execCommand b (Remove (nb, lab))   = removeLens b nb lab
execCommand b (Add (nb, lab, foc)) = addLens b nb (lab, foc)


-- Calculate focussing power of a boxchain
foc :: Boxchain -> Int
foc b = sum                          -- sum factors
      . map (uncurry (*))            -- multiple
      . zip [1..]                    -- zip products with box index
      . map (foldl (+) 0)            -- add up products per box
      . map (map (uncurry (*)))      -- multiply focal length with slot nb
      . map (zip [1..])              -- zip foc lengths with slot nb
      . map (map (\(_, fl) -> fl))   -- discard labels, retain only foc length
      $ b



main = do
  
    filecontents <- readFile "input.txt"
    let input = tok "," $ head . lines $ filecontents


    ------------
    -- Part 1 --
    ------------

    print $ sum . map hash $ input

    ------------
    -- Part 2 --
    ------------

    -- Parse commands
    let commands = map parseCommand input

    -- The list of lense boxes.
    -- This is inefficient, since indexing is in O(n).
    -- For more performance, this could trivially get
    -- replaced by a Vector.
    let empty = replicate 256 []

    -- Apply commands and calculate final focussing power.
    let final = foldl execCommand empty commands
    print $ foc $ final

    print $ "---------- Done. ----------"
