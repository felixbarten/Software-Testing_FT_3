> {-# LANGUAGE FlexibleInstances #-} 

> module Exercise3 where

> import SetOrd
> import Test.QuickCheck
> import Control.Monad
> import System.Random

** Time spent : less than 1h **

** Sample test with quickCheck: quickCheckResult checkDifference **

** Output : *Exercise3> quickCheckResult checkDifference  
+++ OK, passed 100 tests.
Success {numTests = 100, labels = [], output = "+++ OK, passed 100 tests.\n"} **

> instance Arbitrary (Set Int) where
>	arbitrary = liftM list2set arbitrary

> setIntersection :: Set Int -> Set Int -> Set Int
> setIntersection (Set []) (Set ys) = Set []
> setIntersection (Set (x:xs)) (Set ys) | inSet x (Set ys) = insertSet x (setIntersection (Set xs) (deleteSet x (Set ys)) )
>				      | otherwise = setIntersection (Set xs) (Set ys)


> setUnion :: Set Int -> Set Int -> Set Int
> setUnion (Set []) (Set ys) = Set ys
> setUnion (Set (x:xs)) (Set ys) | inSet x (Set ys) =  setUnion (Set xs) (Set ys)
>				 | otherwise = insertSet x (setUnion (Set xs) (deleteSet x (Set ys)) )


> setDifference :: Set Int -> Set Int -> Set Int
> setDifference (Set []) (Set ys) = Set []
> setDifference (Set xs) (Set []) = Set xs
> setDifference (Set (x:xs)) (Set ys) | not (inSet x (Set ys)) = insertSet x (setDifference (Set xs) (Set ys))
>				      | otherwise = setDifference (Set xs) (Set ys)

We know that A'+'A = A. So, for testing:

> checkUnion :: Set Int -> Bool
> checkUnion (Set x) = setUnion (Set x) (Set x) == Set x

We know that A'*'A = A. So, for testing:

> checkIntersection :: Set Int -> Bool
> checkIntersection (Set x) = setIntersection (Set x) (Set x) == Set x

We know that A'*'B = A - (A-B). So, for testing:

> checkDifference :: Set Int -> Set Int -> Bool
> checkDifference (Set x) (Set y) = setIntersection (Set x) (Set y) == setDifference (Set x) (setDifference (Set x) (Set y))
