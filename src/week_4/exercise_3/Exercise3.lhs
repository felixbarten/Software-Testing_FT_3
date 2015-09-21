> module Exercise3 where

> unionSet :: Eq a => Set a -> Set a -> Set a
> unionSet (Set []) (Set ys) = Set ys 
> unionSet (Set (x:xs)) (Set ys) | inSet x (Set ys) = unionSet (Set xs) (Set ys)
>                                | otherwise = insertSet x $ unionSet (Set xs) $ deleteSet x (Set ys)

> intersectSet :: Eq a => Set a -> Set a -> Set a
> intersectSet (Set []) (Set ys) = Set [] 
> intersectSet (Set (x:xs)) (Set ys) | inSet x (Set ys) = insertSet x $ intersectSet (Set xs) (deleteSet x (Set ys)) 
>                                   | otherwise = intersectSet (Set xs) (Set ys)

> differenceSet :: Eq a => Set a -> Set a -> Set a
> differenceSet (Set []) (Set ys) = Set [] 
> differenceSet (Set (x:xs)) (Set ys) | not $ inSet x (Set ys) = insertSet x $ differenceSet (Set xs) (deleteSet x (Set ys)) 
>                                    | otherwise = differenceSet (Set xs) (Set ys)
