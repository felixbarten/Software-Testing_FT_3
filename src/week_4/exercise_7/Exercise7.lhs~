> module Exercise7 where

> import Data.List
> import Test.QuickCheck

*** Time spent: 30mins ***

> test_prop_trans x = trClos (trClos x) == trClos x
> test_prop_symm x = symClos (symClos x) == symClos x

*** sample execution ***

quickCheckResult test_prop_trans

+++ OK, passed 100 tests.


> type Rel a = [(a,a)]

> infixr 5 @@
> 
> (@@) :: Eq a => Rel a -> Rel a -> Rel a
> r @@ s = 
>   nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]



> transR :: Ord a => Rel a -> Bool
> transR ([]) = True
> transR (s) = and [ trans pair (s) | pair <- s ] where
> 	trans (x,y) ( r) = and [ elem (x,v) ( r) | (u,v) <- r, u == y ]


> trClos :: Ord a => Rel a -> Rel a
> trClos rel =  if transR rel then sort (rel) else trClos (union rel (rel@@rel))


> symClos :: Ord a => Rel a -> Rel a
> symClos (x:xs) =  if symmRel (x:xs) then sort (x:xs) else 
>   if x' `elem` xs then symClos (xs ++ [x]) else symClos (xs ++ [x,x'])
>   where x' = (snd x, fst x)


> symmRel :: Eq a => Rel a -> Bool
> symmRel r = all (\(x,y) -> (y,x) `elem` r) r
