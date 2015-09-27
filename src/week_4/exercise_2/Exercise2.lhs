> {-# LANGUAGE FlexibleInstances #-} 

> module Exercise2 where

> import SetOrd
> import System.Random
> import Test.QuickCheck
> import Control.Monad

*** random data generator ***

> generateSet :: IO (Set Int)
> generateSet =  do 
>              seed <- getStdGen
>              length <-getStdRandom(randomR(0,20))
>               let list = take length (randoms seed:: [Int]) 
>              return (Set list)


> instance Arbitrary (Set Int) where
>    arbitrary = liftM list2set arbitrary

Time spent: about two hours