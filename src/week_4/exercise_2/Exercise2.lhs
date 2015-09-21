> {-# LANGUAGE FlexibleInstances #-} 

> module Exercise2 where

> import SetOrd

> import System.Random
> import Test.QuickCheck
> import Control.Monad

Report
=====
_Time Spent_: 1.5 hours

*Mine*

> newtype SetInt  = SetInt ( Set Int ) deriving (Eq,Ord)

> getRandomInt :: Int -> IO Int
> getRandomInt n = getStdRandom (randomR (0,n))

> getIntS :: Int -> Int -> IO (Set Int)
> getIntS k n = do 
>               r <- aux k n
>               return (list2set r)
>   where aux k n = if n == 0 then return [] else
>                   do
>                   x <- getRandomInt k
>                   xs <- aux k (n-1)
>                   return (x:xs)


*QuickCheck*

> instance Arbitrary (Set Int) where
>    arbitrary = liftM list2set arbitrary

