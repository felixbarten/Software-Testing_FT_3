> {-# LANGUAGE FlexibleInstances #-} 

> module Exercise3 where

> import SetOrd
> import Test.QuickCheck
> import Control.Monad

Test Report
==========

The three functions are checked using the quickCheck and the arbitrary definition for integers created during the previous exercise
. The unionSet function already existed so unionSet' was chosen as a name for the new implementation. The following properties are used to validate 
the implementations:

1. The union of a set with itself is equal to the original set
2. The intesection of two sets can be defined using the difference of the two sets

Time-spent: 1 hour


> instance Arbitrary (Set Int) where
>    arbitrary = liftM list2set arbitrary

> unionSet' :: Set Int -> Set Int -> Set Int
> unionSet' (Set []) (Set ys) = Set ys 
> unionSet' (Set (x:xs)) (Set ys) | inSet x (Set ys) = unionSet' (Set xs) (Set ys)
>                                | otherwise = insertSet x $ unionSet' (Set xs) $ deleteSet x (Set ys)

> intersectSet :: Set Int -> Set Int -> Set Int
> intersectSet (Set []) (Set ys) = Set [] 
> intersectSet (Set (x:xs)) (Set ys) | inSet x (Set ys) = insertSet x $ intersectSet (Set xs) (deleteSet x (Set ys)) 
>                                   | otherwise = intersectSet (Set xs) (Set ys)

> differenceSet :: Set Int -> Set Int -> Set Int
> differenceSet (Set []) (Set ys) = Set [] 
> differenceSet (Set (x:xs)) (Set ys) | not $ inSet x (Set ys) = insertSet x $ differenceSet (Set xs) (deleteSet x (Set ys)) 
>                                    | otherwise = differenceSet (Set xs) (Set ys)

> prop_union_idempotence :: Set Int -> Bool  
> prop_union_idempotence x = (unionSet' x x) == x

> prop_intersect_as_difference :: Set Int -> Set Int -> Bool  
> prop_intersect_as_difference x y = (intersectSet  x y) == differenceSet x (differenceSet x y)

> test_idem  = quickCheckResult prop_union_idempotence  

> test_insect_as_diff =quickCheckResult prop_intersect_as_difference 
