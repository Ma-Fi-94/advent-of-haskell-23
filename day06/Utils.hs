module Utils where

-----------
-- Sugar --
-----------

readInt     = (read :: String -> Int)
readInteger = (read :: String -> Integer)


-----------
-- Lists --
-----------

-- Takes a list and groups it into sublists of length n.
-- The last sublist will be shorter if input is not divisible without rest.
groupn :: Int -> [a] -> [[a]]
groupn _ [] = []
groupn n xs = (take n xs) : (groupn n (drop n xs))

-- Like !!, but order of operands is saner
at :: Int -> [a] -> a
at n list = list !! n


-- Tokenise an array into a list of arrays based on delimiters
-- Multiple delimiters are considered as one token.
-- Delimiters at the beginning and end are ignored
tok :: Eq a => [a] -> [a] -> [[a]]
tok _ [] = []
tok delims input@(x:xs)
    |x `elem` delims = tok delims xs
    |otherwise       = token : tok delims rest
  where
    isDelimiter = (`elem` delims)
    token       = takeWhile (not . isDelimiter) input
    rest        = dropWhile isDelimiter $ dropWhile (not . isDelimiter) input


-- Replace all occurences of a list item
repl :: Eq a => a -> a -> [a] -> [a]
repl _ _ [] = []
repl old new (x:xs) = x' : (repl old new xs)
  where
    x' = if (x /= old) then x else new


-- First argument is the prefix to be checked
beginsWith :: Eq a => [a] -> [a] -> Bool
beginsWith [] _          = True
beginsWith _ []          = False
beginsWith (x:xs) (y:ys) = (x == y) && beginsWith xs ys


-- First argument is the suffix to be checked
endsWith :: Eq a => [a] -> [a] -> Bool
endsWith x y = beginsWith (reverse x) (reverse y)


