module Week_2.W2_Derangements.Derangements where

isDerangement:: [Int] -> [Int] -> Bool
isDerangement xs ys = checkItemDerangement xs xs ys

checkItemDerangement:: [Int] -> [Int] -> [Int] -> Bool
checkItemDerangement _ [] [] = True
checkItemDerangement _ _ [] = False
checkItemDerangement [] [] _ = False
checkItemDerangement xs zs ys =
		if ((head ys) `elem` xs) && (head ys /= head zs)
		then checkItemDerangement xs (removeFromList (head zs) zs) (removeFromList (head ys) ys)
		else False

{- *** below wrong version ***

checkItemDerangement:: [Int] -> [Int] -> Int -> Bool
checkItemDerangement [] [] position = True
checkItemDerangement _ [] position = False
checkItemDerangement [] _ position = False
checkItemDerangement xs ys position =
		if (((head ys) `elem` xs) && ((head ys) /= position)) --
		then checkItemDerangement (removeFromList (head ys) xs) (removeFromList (head ys) ys) (position+1)
		else False

-}


removeFromList:: Eq a => a -> [a] -> [a]
removeFromList _ [] = []
removeFromList x (y:ys) 
		| x == y = ys
		| otherwise = y: removeFromList x ys
