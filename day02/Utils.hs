module Utils where

-----------
-- Sugar --
-----------

readInt     = (read :: String -> Int)
readInteger = (read :: String -> Integer)


-----------
-- Lists --
-----------

-- Like !!, but order of operands is saner
at :: Int -> [a] -> a
at n list = list !! n


-- Tokenise an array into a list of arrays based on delimiters
-- Multiple delimiters are considered as one token.
tok :: Eq a => [a] -> [a] -> [[a]]
tok _ []  = []
tok delims xs = token : tok delims rest
  where
    isDelimiter = (`elem` delims)
    token       = takeWhile (not . isDelimiter) xs
    rest        = dropWhile isDelimiter $ dropWhile (not . isDelimiter) xs 


-- First argument is the prefix to be checked
beginsWith :: Eq a => [a] -> [a] -> Bool
beginsWith [] _          = True
beginsWith _ []          = False
beginsWith (x:xs) (y:ys) = (x == y) && beginsWith xs ys


-- First argument is the suffix to be checked
endsWith :: Eq a => [a] -> [a] -> Bool
endsWith x y  = beginsWith (reverse x) (reverse y)


