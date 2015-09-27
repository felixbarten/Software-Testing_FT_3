> {-# LANGUAGE FlexibleInstances #-}


> module Exercise7 where

Testing of symmetric and transitive closure

> import Closures
> import Test.QuickCheck
> import System.Random


There is already a quickcheck generator defined for the Rel datatype so no new implementation is needed
*Exercise7> sample' $ arbitrary :: IO [(Int, Int)]
[(0,0),(-2,-2),(-3,3),(6,3),(2,6),(-4,-9),(-7,-9),(14,-9),(-6,16),(11,15),(-11,-7)]
*Exercise7> sample' $ arbitrary :: IO [(Int, Int)]
[(0,0),(0,-1),(-2,-1),(-3,-1),(-3,5),(-4,3),(8,3),(1,11),(13,14),(-9,-7),(-15,-6)]
*Exercise7> sample' $ arbitrary :: IO [(Int, Int)]
[(0,0),(0,1),(2,3),(3,1),(-4,-4),(7,2),(-10,-7),(-5,-10),(8,-3),(7,-11),(-4,7)]

Test properties

> prop_transitive :: Ord a=> Rel Int -> Bool
> prop_transitive r = ( trans $ trClos r)


> prop_symmetric :: Ord a => Rel Int -> Bool
> prop_symmetric r = symmetric r == ( symmetric $ symClos r)
