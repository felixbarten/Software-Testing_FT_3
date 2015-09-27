> module Exercise5 where
> import Data.List


*** Time spent: 15mins ***

> type Rel a = [(a,a)]


> symClos :: Ord a => Rel a -> Rel a
> symClos (x:xs) =  if symmRel (x:xs) then sort (x:xs) else 
>   if x' `elem` xs then symClos (xs ++ [x]) else symClos (xs ++ [x,x'])
>   where x' = (snd x, fst x)


> symmRel :: Eq a => Rel a -> Bool
> symmRel r = all (\(x,y) -> (y,x) `elem` r) r
