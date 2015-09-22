> module Exercise5 where

> import Data.List
> type Rel a = [(a,a)]

> symClos :: Ord a => Rel a -> Rel a
> symClos r = sort $ foldr (\(x,y) m -> if (y,x) `notElem` m then (y,x):m  else m ) r r   

Time Spent: 20 mins
==================
                                                                  
