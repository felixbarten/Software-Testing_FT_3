module Week_2.W2_Permutations.Permutations where

import Data.List
import System.Random

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = xs `elem` permutations ys && ys `elem` permutations xs

prop_permutation :: Eq a => [a] -> [a] -> Bool
prop_permutation xs ys = perms xs == perms ys

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)
{- 
If you assume that the input list does not contain duplicates it would be easier to check if the elements are permutations?
-}

{-
Properties 
-}


{- 
Testing
-}
testPermutations :: [Int] -> [Int]
testPermutations = undefined

randomFlip :: Int -> IO Int
randomFlip x = do 
   b <- getRandomInt 1
   if b==0 then return x else return (-x)

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


testR :: Int -> Int -> ([Int] -> [Int]) -> ([Int] -> [Int] -> Bool) -> IO ()
testR k n f r = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- genIntList
                  if r xs (f xs) then
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
                  else error ("failed test on: " ++ show xs)
                  
testPost :: ([Int] -> [Int]) -> ([Int] -> Bool) -> IO ()
testPost f p = testR 1 100 f (\_ -> p)
                  
testPre :: ([Int] -> Bool) -> ([Int] -> [Int]) -> IO ()
testPre p f  = testR 1 100 f (\_ -> p)
                  
samelength :: [Int] -> [Int] -> Bool
samelength xs ys = length xs == length ys

prop_ordered :: Ord a => [a] -> Bool
prop_ordered [] = True
prop_ordered (x:xs) = all (>= x) xs && prop_ordered xs

isTrue :: a -> Bool
isTrue _ = True

testRel :: ([Int] -> [Int]) -> ([Int] -> [Int] -> Bool) -> IO ()
testRel f r = testR 1 100 f r 

quicksrt :: Ord a => [a] -> [a]  
quicksrt [] = []  
quicksrt (x:xs) = 
   quicksrt [ a | a <- xs, a < x ]  
   ++ [x]
   ++ quicksrt [ a | a <- xs, a > x ]


                  