module Utils where

-- First argument is the prefix to be checked
beginsWith :: String -> String -> Bool
beginsWith [] _          = True
beginsWith _ []          = False
beginsWith (x:xs) (y:ys) = (x == y) && beginsWith xs ys


-- First argument is the suffix to be checked
endsWith :: String -> String -> Bool
endsWith [] _ = True
endsWith _ [] = False
endsWith x y  = beginsWith (reverse x) (reverse y)


-- Tokenise an array into a list of arrays based on delimiters
-- Multiple delimiters are considered as one token.
tok :: Eq a => [a] -> [a] -> [[a]]
tok _ []  = []
tok delims xs = token : tok delims rest
  where
    isDelimiter = (`elem` delims)
    token       = takeWhile (not . isDelimiter) xs
    rest        = dropWhile isDelimiter $ dropWhile (not . isDelimiter) xs 


-- Like !!, but order of operands is saner
at :: Int -> [a] -> a
at n list = list !! n

-- Sugar
readInt :: String -> Int
readInt = read