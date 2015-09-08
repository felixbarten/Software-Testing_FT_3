module Week_2.W2_Permutations.Permutations where

-- is the 2nd one permutation of 1st one?

isPermutation:: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation _ [] = False
isPermutation [] _ = False
isPermutation xs (y:ys)
		| y `elem` xs = (isPermutation (removeFromList y xs) ys)
		| otherwise = False


removeFromList:: Eq a => a -> [a] -> [a]
removeFromList _ [] = []
removeFromList x (y:ys) 
		| x == y = ys
		| otherwise = y: removeFromList x ys
