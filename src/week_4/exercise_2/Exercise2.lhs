> {-# LANGUAGE FlexibleInstances #-}

> module Exercise2 where

> import System.Random
> import Data.List
> import SetOrd
> import Test.QuickCheck


Implement a random data generator for the datatype Set Int, where Set is as defined in SetOrd.hs. First do this from scratch, next give a version that uses QuickCheck to random test this datatype.
(Deliverables: two random test generators, indication of time spent.)



> instance Arbitrary (Set Int) where
>		arbitrary = do
>			x <- arbitrary
>			return (list2set (x))

> generateList ::  Int -> Int-> Int -> IO [Int]
> generateList _ _ 0 = return []
> generateList min max total = do
>		x  <- nRandom min max
>		xs <- generateList min max (total-1)
>		return (x:xs)

> generateSet :: Int -> Int-> Int ->IO (Set Int)
> generateSet min max total = do 
>		x <- generateList min max total 
>		return (list2set x)

> nRandom :: Int -> Int -> IO Int
> nRandom min max= do
>   number <- randomRIO (min,max) :: IO Int
>   return number



