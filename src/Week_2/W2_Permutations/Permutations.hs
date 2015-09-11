module Week_2.W2_Permutations.Permutations where

import Data.List
import System.Random


isPermutation:: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation _ [] = False
isPermutation [] _ = False
isPermutation xs (y:ys)
		| y `elem` xs = (isPermutation (removeFromList y xs) ys)
		| otherwise = False

--isPermutation :: Eq a => [a] -> [a] -> Bool
--isPermutation xs ys = xs `elem` permutations ys && ys `elem` permutations xs

removeFromList:: Eq a => a -> [a] -> [a]
removeFromList _ [] = []
removeFromList x (y:ys) 
		| x == y = ys
		| otherwise = y: removeFromList x ys

-- precondition below --

hasDuplicates:: Eq a => [a] -> Bool 
hasDuplicates xs = length xs /= length (nub xs)

-- random staff + random list generator + permutations --

getListPermutations :: IO ([Int],[[Int]])
getListPermutations = do 
   x <-  genIntList'
   let m =  nub x
   return (m, permutations m)

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

randomFlip :: Int -> IO Int
randomFlip x = do 
   b <- getRandomInt 1
   if b == 0 then return x else return (-x)

genIntList' :: IO [Int]
genIntList' = do 
  k <- getRandomInt 77
  n <- getRandomInt 8
  getIntL k n

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)

-- testing below --

-- sample execution: testPost isPermutation sameLength

testPost :: ([Int] -> [Int] -> Bool) -> ([Int] -> [Int]-> Bool) -> IO ()
testPost f p = testR' 1 300 f p --f (\_ -> p)

samelength :: [a] -> [a] -> Bool
samelength xs ys = length xs == length ys

testR' :: Int -> Int -> ([Int] -> [Int] -> Bool)
                    -> ([Int] -> [Int] -> Bool) -> IO ()
testR' k n f r = if k == n then print (show n ++ " tests passed")
                else do
                  (xs, xss) <- getListPermutations
                  l <- getRandomInt (length xss -1 ) 
                  let ys =  xss !! l
                  if r xs ys == f xs ys then
                    do print ("*** pass on: " ++ show xs ++ ", " ++ show ys)
                       testR' (k+1) n f r
                  else error ("*** failed test on: " ++ show xs ++ ", " ++ show ys)
