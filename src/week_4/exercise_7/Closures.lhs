> module Closures where

This module contains copied definitions of the transitive and symmetric closures from exercise 5 & 6

> import Data.List
> import Data.Ord


> type Rel a = [(a,a)]


> infixr 5 @@

> (@@) :: Eq a => Rel a -> Rel a -> Rel a
> r @@ s = 
>   nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

Transitive closure so Union of r and R composition R 

> trClos :: Ord a => Rel a -> Rel a 
> trClos r = if trans r then sortBy (comparing fst) r else trClos (union r (r@@r))


> trans :: Ord a => Rel a -> Bool
> trans r = all (`elem` r) (r@@r)

Symmetric closure

> symClos :: Ord a => Rel a -> Rel a 
> symClos r= sortBy (comparing fst) $ foldr (\(x,y) acc -> if (y,x) `elem` r then (x,y):acc else (x,y):(y,x):acc) [] r

> symmetric :: Eq a => Rel a -> Bool
> symmetric r = all (\(x,y) -> (y,x) `elem` r) r