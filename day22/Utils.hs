module Utils where

import Data.Maybe
import qualified Data.Map as Map

-------------
-- Reading --
-------------

readInt     = (read :: String -> Int)
readInteger = (read :: String -> Integer)
readDouble  = (read :: String -> Double)


------------
-- Tuples --
------------

fst3 (a, _, _) = a
snd3 (_, b, _) = b
thi3 (_, _, c) = c

fst4 (a, _, _, _) = a
snd4 (_, b, _, _) = b
thi4 (_, _, c, _) = c
fou4 (_, _, _, d) = d


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

-- Like dropWhile, but also drops the first element
-- that does not fulfill the predicate any more.
dropWhileIncl :: Eq a => (a -> Bool) -> [a] -> [a]
dropWhileIncl p = drop 1 . dropWhile p


-- Find the first recurring element and return
-- the index of its first and its second occurrence
firstRecElem :: Ord a => [a] -> Maybe (Int, Int)
firstRecElem xs = go Map.empty 0 xs
  where
    go _ _ []        = Nothing
    go seen i (x:xs) = case (Map.lookup x seen) of
                             Just j  -> Just (j,i)
                             Nothing -> go (Map.insert x i seen) (i+1) xs


-- Cartesian product of two lists
cart :: [a] -> [b] -> [(a,b)]
cart x y = (,) <$> x <*> y


-- This should actually be present in the base lib, but isn't.
-- Total version of !!.
(!?) :: [a] -> Int -> Maybe a
[] !? n     = Nothing
(x:_) !? 0  = Just x
(_:xs) !? n = xs !? (n-1)


-- Takes a list and groups it into sublists of length n.
-- The last sublist will be shorter if input is not divisible without rest.
groupn :: Int -> [a] -> [[a]]
groupn _ [] = []
groupn n xs = (take n xs) : (groupn n (drop n xs))


-- Tokenise an array into a list of arrays based on delimiters
-- Multiple delimiters are considered as one delimiter.
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

