import Data.Char (isDigit)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (isNothing, isJust, fromJust)
import Utils (tok, dropWhileIncl, (&&&))

-- Sugar
type WorkflowMap  = Map WorkflowName Workflow
type WorkflowName = String
type Workflow     = [Either Condition WorkflowName]
type Condition    = (TupleIdx, Operation, Operand, WorkflowName)
type TupleIdx     = Int
data Operation    = Less | Greater deriving (Show, Eq)
type Operand      = Int

-- Inclusive at both borders, i.e. closed interval!
type Range        = (Int, Int) 
type Range4       = [Range]


-- Parse all workflow lines from the input file
parseInput :: [String] -> WorkflowMap
parseInput = Map.fromList . map parseWorkflow . (!!0) . tok [""]


-- Parse an input line from the first block of the file,
-- that describes a workflow.
parseWorkflow :: String -> (WorkflowName, Workflow)
parseWorkflow inp = (name, steps)
  where
    name  = takeWhile (/='{') $ inp
    steps = map parseStep . tok "," . init . dropWhileIncl (/='{') $ inp
      where
        parseStep s
            |'<' `elem` s = Left  (partIdx s, Less,    operand s, goto s)
            |'>' `elem` s = Left  (partIdx s, Greater, operand s, goto s)
            |otherwise    = Right s
        partIdx s = case (head s) of {'x' -> 0; 'm' -> 1; 'a' -> 2; 's' -> 3}
        operand   = read . takeWhile (isDigit) . dropWhileIncl (`notElem` "><")
        goto      = takeWhile (/=',') . dropWhileIncl (/=':')


-- Calculate the number of elements in a given Range4.
size4 :: Range4 -> Int
size4 r4 = product $ map size r4
  where
    size (l, h) = h - l + 1


-- Split a given Range based on an inequality (LT/GT c).
-- Returns a tuple of two Maybe Range.
-- Left element is the Range that fulfils the conditions,
-- right element is the Range that does not.
split :: Range -> Operation -> Int -> (Maybe Range, Maybe Range)
split (a, b) Less c
    |c < a          = (Nothing, Just (a, b))
    |b < c          = (Just (a, b), Nothing)
    |otherwise      = (Just (a, c - 1), Just (c, b))
split (a, b) Greater c
    |c < a          = (Just (a,b), Nothing)
    |b < c          = (Nothing, Just (a,b))
    |otherwise      = (Just (c + 1, b), Just (a, c))


-- Given a Range4, a coordinate index 0...3 and an inequality,
-- try to cut the Range4 into two. Returns a tuple of two Maybe Range4.
-- Left element is the Range4 that fulfils the conditions,
-- right element is the Range4 that does not.
split4 :: Range4 -> TupleIdx -> Operation -> Int -> (Maybe Range4, Maybe Range4)
split4 r4 idx op cut = (part2Range4 rgTrue, part2Range4 rgFalse)
  where
    parts             = split (r4!!idx) op cut
    (rgTrue, rgFalse) = (&&&) fst snd parts
    part2Range4 p     = if   isNothing p
                        then Nothing
                        else Just ((take idx r4)
                                ++ [fromJust p]
                                ++ (drop (idx + 1) r4))


-- Find the accepted region of a Range4
find :: WorkflowMap -> Workflow -> Range4 -> Int
find _   ((Right "A"):_) r4    = size4 r4
find _   ((Right "R"):_) r4    = 0
find wfm ((Right s):_)   r4    = find wfm (wfm Map.! s) r4
find wfm ((Left (idx, op, cut, goto)):steps) r4 = fulfils + notFulfils
  where
    parts      = split4 r4 idx op cut
    fulfils    = if   isJust (fst parts)
                 then find wfm [Right goto] (fromJust (fst parts))
                 else 0
    notFulfils = if   isJust (snd parts)
                 then find wfm steps (fromJust (snd parts))
                 else 0


main = do
  
    filecontents <- readFile "input.txt"
    let workflowmap   = parseInput . lines $ filecontents
    let firstworkflow = workflowmap Map.! "in"

    print $ find workflowmap firstworkflow $ replicate 4 (1, 4000)

    

    print $ "---------- Done. ----------"
