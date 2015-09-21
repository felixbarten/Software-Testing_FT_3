> {-# LANGUAGE FlexibleInstances #-}


> module Exercise2 where


> import SetOrd
> import Test.QuickCheck
> import System.Random
> import Data.List


Generates a random set with a length between zero and fifty

> genSets :: IO (Set Int)
> genSets =  do 
>              seed <-getStdGen
>              length <-getStdRandom(randomR(0,50))
>              let rlist = take length $ (randoms seed :: [Int])
>              let set = Set rlist
>              return $ set


> randomlist :: Int -> StdGen -> [Int]
> randomlist n = take n . unfoldr (Just . random)

Need a possible workaround around the "flexible instances"

> instance Arbitrary (Set Int)  where
>   arbitrary  = do
>              x <- arbitrary :: Gen Int
>              y <- listOf $ choose(0,x)
>              return $ list2set y

Dummy test to view output (could also be accomplished by sampling the output of the arbitrary of course)

> seeOutput :: Set Int -> Bool
> seeOutput x = show x == show x
    