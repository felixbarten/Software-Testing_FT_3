> module Exercise3 where

> import System.Random
> import Data.List
> import SetOrd
> import Test.QuickCheck

Implement operations for set intersection, set union and set difference, for the datatype Set defined in SetOrd.hs. Next, use automated testing to check that your implementation is correct. First use your own generator, next use QuickCheck.
(Deliverables: implementations, test properties, short test report, indication of time spent.)


> setInter :: Eq a => Set a -> Set a -> Set a 
> setInter (Set xs) (Set ys) = Set [ x | x <- xs , x `elem` ys]

> setUnion :: Ord a => Set a -> Set a -> Set a 
> setUnion (Set xs) (Set ys) = list2set(xs ++ ys)

> setDiff :: Eq a => Set a -> Set a -> Set a
> setDiff (Set xs) (Set ys) = Set (filter(not . ( `elem` ys)) xs)