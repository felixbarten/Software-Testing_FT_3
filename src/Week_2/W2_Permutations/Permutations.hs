module Week_2.W2_Permutations.Permutations where

import Data.List
import System.Random
import Week_2.Testing

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = xs `elem` permutations ys && ys `elem` permutations xs

prop_permutation :: Ord a => [a] -> [a] -> Bool
prop_permutation xs ys = sort (perms xs) == sort (perms ys)

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)
{- 
If you assume that the input list does not contain duplicates it would be easier to check if the elements are permutations?

No, the amount of permutations doesn't change (still 2^n) but the permutations might be equal to each other. For example:
perms [5,5,5] = [[5,5,5],[5,5,5],[5,5,5],[5,5,5],[5,5,5],[5,5,5]]
-}


{- 
Testing
-}

-- general not automated tests

testIsPermutation :: ([Int], [Int], Bool) -> Bool
testIsPermutation (xs, ys, b) = isPermutation xs ys == b


permutationTests :: [Test]
permutationTests = [ Test "Valid Tests" testIsPermutation [
    ([1,3,5], [5,3,1], True),
    ([2,3,4,6,7,8,9], [9,8,7,6,4,2,3], True),
    ([0,1,5], [1,0,5], True),
    ([0,2,-2,2,0],[2,-2,0,2,0], True),
    ([-5,4,3,7],[7,-5,3,4], True),
    ([10,11,12,13,14],[14,12,11,10,13], True),
    ([8,4,2,1],[4,1,2,8], True),    
    ([45,32,87,54],[54,32,87,45], True)                                           ]
        ]

noPermutationTests :: [Test]
noPermutationTests = [ Test "Invalid Tests" testIsPermutation [
    ([1,3,5], [5,3,10], False),
    ([2,3,4,6,7,8,9], [9,8,7,6,4,2,3,20], False),
    ([0],[0,1], False),
    ([45,32,87,54],[54,32,87,54,45], False)                                           ]
                            
            ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = permutationTests ++ noPermutationTests

{- (mostly) automated tests -}


randomFlip :: Int -> IO Int
randomFlip x = do 
   b <- getRandomInt 1
   if b == 0 then return x else return (-x)

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))
                    
genIntList :: IO [Int]
genIntList = do 
  k <- getRandomInt 5
  n <- getRandomInt 5
  getIntL k n
  
getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)                


testR :: Int -> Int -> ([Int] -> [Int] -> Bool) -> ([Int] -> [Int] -> Bool) -> IO ()
testR k n f r = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- genIntList
                  ys <- genIntList
                  if r xs ys == (f xs ys) then
                    do print ("pass on: " ++ show xs)
                       testR (k+1) n f r
                  else error ("failed test on: " ++ show xs)
                  
testPerms :: Int -> Int -> ([Int] -> [Int] -> Bool) -> ([Int] -> [Int] -> Bool) -> IO()
testPerms k n f r = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- genIntList
                  ys <- genIntList
                  if r xs ys  == (f xs ys) then
                    do print ("pass on: expected permutation?:" ++ show  (r xs ys || f xs ys) ++ " from: "++ show xs ++ " and " ++ show ys)
                       testPerms (k+1) n f r
                  else error ("failed test on: " ++ show xs ++ " and " ++ show ys )
                  
samelength :: [Int] -> [Int] -> Bool
samelength xs ys = length xs == length ys

prop_ordered :: Ord a => [a] -> Bool
prop_ordered [] = True
prop_ordered (x:xs) = all (>= x) xs && prop_ordered xs

prop_samelength :: [a] -> [a] -> Bool
prop_samelength xs ys  = length xs == length ys

isTrue :: a -> Bool
isTrue _ = True

testRel :: ([Int] -> [Int] -> Bool) -> ([Int] -> [Int] -> Bool) -> IO ()
testRel f r = testR 1 100 f r 

quicksrt :: Ord a => [a] -> [a]  
quicksrt [] = []  
quicksrt (x:xs) = 
   quicksrt [ a | a <- xs, a < x ]  
   ++ [x]
   ++ quicksrt [ a | a <- xs, a > x ]


                  