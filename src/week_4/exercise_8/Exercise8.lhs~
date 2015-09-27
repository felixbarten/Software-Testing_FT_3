> module Exercise8 where


> import Data.List

*** Time spent: 20mins ***

** The transitive closure of a relation R and the transitive closure of the symmetric closure of R are different.
	Below, a simple example is shown that confirms this.	**

Relation: [(1,2),(2,3)]


*Exercise8> symClos $ trClos [(1,2),(2,3)]
[(1,2),(1,3),(2,1),(2,3),(3,1),(3,2)]


*Exercise6> trClos $ symClos [(1,2),(2,3)]
[(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]



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

> symClos :: Ord a => Rel a -> Rel a
> symClos (x:xs) =  if symmRel (x:xs) then sort (x:xs) else 
>   if x' `elem` xs then symClos (xs ++ [x]) else symClos (xs ++ [x,x'])
>   where x' = (snd x, fst x)


> symmRel :: Eq a => Rel a -> Bool
> symmRel r = all (\(x,y) -> (y,x) `elem` r) r
