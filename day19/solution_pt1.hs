import Data.Char (isDigit)
import qualified Data.Map as Map
import Data.Map (Map)
import Utils (tok, dropWhileIncl)

-- Sugar
type Part         = [Int]
type PartIdx      = Int

type WorkflowMap  = Map WorkflowName Workflow
type WorkflowName = String
type Workflow     = [Either Condition WorkflowName]
type Condition    = (PartIdx, Operation, Operand, WorkflowName) 
data Operation    = Less | Greater deriving (Show, Eq)
type Operand      = Int


-- Parse all lines from the input file
parseInput :: [String] -> (WorkflowMap, [Part])
parseInput ls = (wfm, parts)
  where
    wfm    = Map.fromList . map parseWorkflow $ blocks!!0
    parts  = map parsePart $ blocks!!1
    blocks = tok [""] ls


-- Parse an input line from the first block of the file,
-- that describes a workflow.
parseWorkflow :: String -> (WorkflowName, Workflow)
parseWorkflow inp = (name, steps)
  where
    name  = takeWhile (/='{') $ inp
    steps = map parseStep . tok "," . init . dropWhileIncl (/='{') $ inp
      where
        parseStep s
            |'<' `elem` s = Left  (partIdx s, Less, operand s, goto s)
            |'>' `elem` s = Left  (partIdx s, Greater, operand s, goto s)
            |otherwise    = Right s
        partIdx s = case (head s) of {'x' -> 0; 'm' -> 1; 'a' -> 2; 's' -> 3}
        operand   = read . takeWhile (isDigit) . dropWhileIncl (`notElem` "><")
        goto      = takeWhile (/=',') . dropWhileIncl (/=':')


-- Parse an input line from the second block of the file,
-- that describes a part.
parsePart :: String -> Part
parsePart inp = numbers
  where
    toks    = map read . tok "{},xmas=" $ inp
    numbers = [toks!!0, toks!!1, toks!!2, toks!!3]


-- Given the workflowmap, apply a given workflow to a given part.
-- Returns the ultimate decision, i.e. Accept or Reject.
decide :: WorkflowMap -> Workflow -> Part -> Bool
decide _ ((Right "A"):_) _ = True
decide _ ((Right "R"):_) _ = False
decide wfm ((Right s):_) pt = decide wfm (wfm Map.! s) pt
decide wfm ((Left (idx, op, opd, goto)):steps) pt
    |op == Less    && pt!!idx < opd = decide wfm [(Right goto)] pt
    |op == Greater && pt!!idx > opd = decide wfm [(Right goto)] pt
    |otherwise                      = decide wfm steps pt


main = do
  
    filecontents <- readFile "input.txt"
    let (workflowmap, parts) = parseInput . lines $ filecontents
    let firstworkflow        = workflowmap Map.! "in"

    print $ sum
          . map sum
          . filter (decide workflowmap firstworkflow)
          $ parts

    print $ "---------- Done. ----------"
