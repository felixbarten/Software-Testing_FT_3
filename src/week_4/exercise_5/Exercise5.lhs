> module Exercise5 where

> import System.Random
> import Data.List
> import Test.QuickCheck

> type Rel a = [(a,a)]

> symClos :: Ord a => Rel a -> Rel a
> symClos r = sort $ foldr (\(x,y) m -> if (y,x) `notElem` m then (y,x) : m else m) r r

*Exercise5> symClos [(1,2),(2,1),(2,3),(1,5)]
[(1,2),(1,5),(2,1),(2,3),(3,2),(5,1)]

Time spent: 30 mins