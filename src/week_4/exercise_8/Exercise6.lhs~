> module Exercise6 where

> import Data.List

*** Time spent: 40mins ***


> type Rel a = [(a,a)]

> infixr 5 @@
> 
> (@@) :: Eq a => Rel a -> Rel a -> Rel a
> r @@ s = 
>   nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]


{- transR method found on book ch. 5.3 -}

> transR :: Ord a => Rel a -> Bool
> transR ([]) = True
> transR (s) = and [ trans pair (s) | pair <- s ] where
> 	trans (x,y) ( r) = and [ elem (x,v) ( r) | (u,v) <- r, u == y ]


> trClos :: Ord a => Rel a -> Rel a
> trClos rel =  if transR rel then sort (rel) else trClos (union rel (rel@@rel))
