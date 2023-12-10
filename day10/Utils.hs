module Utils where

-----------
-- Sugar --
-----------

readInt     = (read :: String -> Int)
readInteger = (read :: String -> Integer)
readDouble  = (read :: String -> Double)


------------------
-- Arrow things --
------------------

-- Apply function to the first element of a 2-tuple
first :: (a -> c) -> (a, b) -> (c, b)
first = \f (x, y) -> (f x, y)


-- Apply funcion to the second element of a 2-tuple
second :: (b -> c) -> (a, b) -> (a, c)
second = \f (x, y) -> (x, f y)


-- Fanout for functions
(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) = \f g x -> (f x, g x)


-- Split for functions
(***) :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
(***) = \f g (x, y) -> (f x, g y)


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
repl old new = map (\x -> if x /= old then x else new)
