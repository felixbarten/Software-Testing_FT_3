> {-# LANGUAGE FlexibleInstances #-} 
> module Exercise7 where

> import Data.List
> import Test.QuickCheck

> type Rel a = [(a,a)]

> prop_already_transitive x =  trClos'(trClos' x) == trClos' x 
> prop_already_symmetric x = symClos'(symClos' x) == symClos' x

> testSymm = quickCheckResult prop_already_symmetric 
> testTrans = quickCheckResult prop_already_transitive

Test Report
==========

Both functions are tested using QuickCheck. Two functions were created to call the symClos and trClos with more specific types so that QuickCheck can randomly generate values for the test.
The tested properties are: 

1. The transitive closure of a transitive relation is itself
2. The symmetric closure of a symmetric closure is itself

*Time spent:* 1.5 hours




Exercise 5

> symClos :: Ord a => Rel a -> Rel a
> symClos r = nub $ sort $ foldr (\(x,y) m -> if (y,x) `notElem` m then (y,x):m  else m ) r r   

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
> trClos' x =  trClos x

> transitive' :: Rel Int -> Bool
> transitive' x = transitive x

> transitive :: Ord a => Rel a -> Bool
> transitive [] = True
> transitive s = and [ trans pair s | pair <- s ] where 
>                trans (x,y) r = and [ elem (x,v) r | (u,v) <- r, u == y ]  

> trClos :: Ord a => Rel a -> Rel a 
> trClos r = nub $ fix (\f' x -> if transitive x then x else x ++ x @@ x) r
