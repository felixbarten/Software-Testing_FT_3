> module Exercise6 where

> import Data.List
> import Data.Ord

> type Rel a = [(a,a)]

> infixr 5 @@
> 
> (@@) :: Eq a => Rel a -> Rel a -> Rel a
> r @@ s = nub [(x,z) | (x,y) <- r, (w,z) <- s, y == w]

trans relation = (1,2), (2,3)
then there must be a (1,3)


> transR :: Ord a => Rel a -> Bool
> transR ([]) = True
> transR (s) = and [ trans pair (s) | pair <- s ] where
> 		trans (x,y) ( r) = and [ elem (x,v) ( r) | (u,v) <- r, u == y ]

> trClos :: Ord a => Rel a -> Rel a
> trClos rel =  if transR rel then sort (rel) else trClos (union rel (rel@@rel))

Ok, modules loaded: Exercise6.
*Exercise6> let r = [(1,2),(2,3)]
*Exercise6> r
[(1,2),(2,3)]
*Exercise6> r @@ r
[(1,3)]
*Exercise6> r
[(1,2),(2,3)]

way tooo long