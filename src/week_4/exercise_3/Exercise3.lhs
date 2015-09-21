> module Exercise3 where


> import SetOrd
> import Exercise2
> import Test.QuickCheck


> delete :: Eq a => a -> [a] -> [a]
> delete x [] = []
> delete x (y:ys) | x == y = ys
>                | otherwise = y : delete x ys


> difference :: Eq a => [a]->[a] -> [a]
> difference xs [] = xs
> difference [] ys = []
> difference xs (y:ys) = difference (delete y xs) ys


4.  Exercise 4.53 Write functions genUnion and genIntersect for generalized list
union and list intersection. The functions should be of type [[a]]-> [a]. They
take a list of lists as input and produce a list as output. Note that genIntersect is
undefined on the empty list of lists (compare the remark about the presupposition
of generalized intersection on page 134).


> union :: Eq a => [a] -> [a] -> [a]
> union [] ys = ys
> union (x:xs) ys = x : union xs (delete x ys)
 
 
> genUnion :: Eq a => [[a]] ->[a]
> genUnion [] = []
> genUnion (xs:xss) = xs `union` genUnion xss

 
> intersect :: Eq a => [a] -> [a] -> [a]
> intersect [] s = []
> intersect (x:xs) s | x `elem` s = x : intersect xs s
>                    | otherwise = xs `intersect` s
 
> genIntersect :: Eq a => [[a]] ->[a]
> genIntersect [] = error "list of lists cannot be empty"
> genIntersect [xs] = xs
> genIntersect (xs:xss) = xs `intersect` genIntersect xss
 
 
 5.  Exercise 4.54 Give implementations of the operations
 unionSet, intersectSet and differenceSet,
 in terms of inSet, insertSet and deleteSet.


> intersectSet :: Set Int -> Set Int -> Set Int
> intersectSet (Set []) (Set ys) = Set []
> intersectSet (Set (x:xs)) (Set ys) | inSet x  (Set ys) = insertSet  x $ intersectSet (Set xs) (deleteSet x (Set ys))
>                                    | otherwise = intersectSet (Set xs) (Set ys)
 
 
> differenceSet :: Set Int -> Set Int -> Set Int
> differenceSet (Set []) ys = Set []
> differenceSet  (Set (x:xs)) (Set ys) | not $ inSet x (Set ys)  = insertSet x $ differenceSet (Set xs) (deleteSet x (Set ys))
>                                      | otherwise = differenceSet (Set xs) (Set ys)


Testing properties


> prop_intersect_as_diff :: Set Int -> Set Int -> Bool
> prop_intersect_as_diff a b = intersectSet a b == differenceSet a (differenceSet a b) 

