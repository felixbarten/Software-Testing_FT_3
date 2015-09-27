> module Exercise6 where

> import Data.List
> type Rel a = [(a,a)]

> fix :: (a -> a) -> a
> fix f = let x = f x in x

> infixr 5 @@
> 
> (@@) :: Eq a => Rel a -> Rel a -> Rel a
> r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]


Transtivity = xRy ^ yRz => xRz

> transitive :: Ord a => Rel a -> Bool
> transitive [] = True
> transitive s = and [ trans pair s | pair <- s ] where 
>                trans (x,y) r = and [ elem (x,v) r | (u,v) <- r, u == y ]  

> trClos :: Ord a => Rel a -> Rel a 
> trClos r = fix (\f' x -> if transitive x then x else x ++ x @@ x) r

Time spent: 1.5 hours
