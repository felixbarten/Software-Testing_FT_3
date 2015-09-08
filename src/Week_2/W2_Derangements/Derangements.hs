module Week_2.W2_Derangements.Derangements where

--isDerangement:: Eq a => [a] -> [a] -> Bool
isDerangement:: [Int] -> [Int] -> Bool
isDerangement xs ys = checkItemDerangement xs ys 0

checkItemDerangement:: [Int] -> [Int] -> Int -> Bool
checkItemDerangement [] [] position = True
checkItemDerangement _ [] position = False
checkItemDerangement [] _ position = False
checkItemDerangement xs ys position =
		--if (position == (length ys) - 1)
		--then if (xs == ys && (head ys) /= position) -- ys!!position
		--	then True
		--	else False
		if (((head ys) `elem` xs) && ((head ys) /= position)) --
		then checkItemDerangement (removeFromList (head ys) xs) (removeFromList (head ys) ys) (position+1)
		else False

--isDerangement [] [] = True
--isDerangement _ [] = False
--isDerangement [] _ = False
--isDerangement xs ys = (isPermutation xs ys) && [(y!!n /= n) | y<- ys, n<-[0..length[ys]-1]]
--isDerangement xs (y:ys)
--		| y `elem` xs
--		if (isPermutation xs ys)
--		then validateDerangement ys 0
--		else False

validateDerangement:: Eq a => [a] ->  a -> Bool
validateDerangement n l = if n == []
				then True
				else False

--validateDerangement [] position = True
--validateDerangement (y:ys) position = ((y!!position) /= position) && (validateDerangement ys (position+1))
			--if (y!!startList /= startList)
			--then validateDerangement ys startList+1
			--else False


removeFromList:: Eq a => a -> [a] -> [a]
removeFromList _ [] = []
removeFromList x (y:ys) 
		| x == y = ys
		| otherwise = y: removeFromList x ys
