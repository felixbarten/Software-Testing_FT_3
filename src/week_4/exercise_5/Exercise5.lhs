> module Exercise5 where


> import Data.List
> import Data.Ord


> type Rel a = [(a,a)]


sym closure... so if Rxy check Ryx if not Ryx insert Ryx

UGLY implementation, but hey it works ;) 
It's not ordered though

> symCloss :: Ord a => Rel a -> Rel a
> symCloss (x:xs) =  if symmetric (x:xs) then (x:xs) else 
>   if x' `elem` xs then symCloss (xs ++ [x]) else symCloss (xs ++ [x,x'])
>   where x' = (snd x, fst x)

Nice implementation ;) 

> symClos :: Ord a => Rel a -> Rel a 
> symClos r= sortBy (comparing fst) $ foldr (\(x,y) acc -> if (y,x) `elem` r then (x,y):acc else (x,y):(y,x):acc) [] r

Definition of symmetry all rxy must contain ryx

> symmetric :: Eq a => Rel a -> Bool
> symmetric r = all (\(x,y) -> (y,x) `elem` r) r

Time spent: 30 minutes on ugly implemenation, 5 minutes to fix it to a nice implementation