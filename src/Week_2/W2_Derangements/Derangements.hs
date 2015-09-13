module Week_2.W2_Derangements.Derangements where

import Testing

permutations :: [a] ->[[a]]
permutations [] = [[]]
permutations (x:xs) = concatMap (insrt x) (permutations xs)
                      where insrt x [] = [[x]]
                            insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = all (`elem` ys) xs && all(`elem` xs) ys 

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = isPermutation xs ys  && helper xs ys
                      where helper [] [] = True
                            helper (x:xs) (y:ys) = x /= y && helper xs ys 

deran :: Integer -> [[Integer]]
deran n = filter (isDerangement list) $ permutations list 
          where list = [0..n-1]

-- Can be used to compare the length of two lists
samelength :: [a] -> [a] -> Bool
samelength xs ys = length xs == length ys

-- Tests

testIsDerangement :: Eq a => (Bool,[a],[a]) -> Bool
testIsDerangement (r, xs, ys)  = isDerangement xs ys == r

isDerangementTests :: [Test]
isDerangementTests = [ Test "isDerangement Test" testIsDerangement [(True,[1,2,3], [3,1,2]), (False, [1,2,3], [1,2,4]), (False, [1,2], [3,4]), (True,[5,1,6,7], [7,6,5,1])]]

testDeran :: (Integer,[[Integer]]) -> Bool
testDeran (n, xss)  = deran n == xss

deranTests :: [Test]
deranTests =  [ Test "deran test" testDeran
             [(0,[[]] ), (2, []), (3,[[2,1]]), (5,[[2,3,4,1],[3,1,4,2],[3,4,1,2],[3,4,2,1],[2,1,4,3],[2,4,1,3],[4,1,2,3],[4,3,1,2],[4,3,2,1]])]]

allTests :: [Test]
allTests = deranTests ++ isDerangementTests

-- Questions: Is it really possible to automate this?
-- Time spent: 2 hours

