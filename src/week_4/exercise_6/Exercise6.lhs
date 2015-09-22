> module Exercise6 where


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

Time spent: 30 minutes