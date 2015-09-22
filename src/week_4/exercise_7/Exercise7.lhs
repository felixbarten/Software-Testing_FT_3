> {-# LANGUAGE FlexibleInstances #-} 
> module Exercise7 where

> import Data.List
> import Test.QuickCheck

> type Rel a = [(a,a)]

> prop_already_transitive x =  trClos'(trClos' x) == trClos' x 
> prop_already_symmetric x = symClos'(symClos' x) == symClos' x
> prop_symm_composition x = symClos' (r @@ r @@ r) == r @@ r @@ r 
>                           where r = symClos' x

Exercise 5

> symClos :: Ord a => Rel a -> Rel a
> symClos r = sort $ foldr (\(x,y) m -> if (y,x) `notElem` m then (y,x):m  else m ) r r   

> symClos' :: Rel Int -> Rel Int
> symClos' x = symClos x
                                                                        
Exercise 6
                                                                        
> fix :: (a -> a) -> a
> fix f = let x = f x in x

> infixr 5 @@
> 
> (@@) :: Eq a => Rel a -> Rel a -> Rel a
> r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]


> trClos' :: Rel Int -> Rel Int
> trClos' x = trClos x

> transitive' :: Rel Int -> Bool
> transitive' x = transitive x

Transtivity = xRy ^ yRz => xRz

> transitive :: Ord a => Rel a -> Bool
> transitive [] = True
> transitive s = and [ trans pair s | pair <- s ] where 
>                trans (x,y) r = and [ elem (x,v) r | (u,v) <- r, u == y ]  

> trClos :: Ord a => Rel a -> Rel a 
> trClos r = fix (\f' x -> if transitive x then x else x ++ x @@ x) r
