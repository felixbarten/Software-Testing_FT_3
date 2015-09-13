module Week_2.W2_Derangements.Derangements where

import Data.List
import System.Random
import Week_2.Testing

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys =  ys `elem` derangement xs
-- ^ checks whether ys is a derangement of xs

derangement :: Eq a => [a] -> [[a]]
derangement xs =  filter (and . zipWith (/=) xs) $ permutations xs

deran :: Integer -> [[Integer]]
deran n = derangement [0..n-1]

{- 
Testing
-}

permutations2 :: [a] ->[[a]]
permutations2 [] = [[]]
permutations2 (x:xs) = concatMap (insrt x) (permutations xs)
                      where insrt x [] = [[x]]
                            insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)
                            
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = all (`elem` ys) xs && all(`elem` xs) ys 

isDerangement2 :: Eq a => [a] -> [a] -> Bool
isDerangement2 xs ys = isPermutation xs ys  && helper xs ys
                      where helper [] [] = True
                            helper (x:xs) (y:ys) = x /= y && helper xs ys 

deran2 :: Integer -> [[Integer]]
deran2 n = filter (isDerangement2 list) $ permutations2 list 
          where list = [0..n-1]

-- general not automated tests

testIsDerangement :: ([Int], [Int], Bool) -> Bool
testIsDerangement (xs, ys, b) = isDerangement xs ys == b


derangementTests :: [Test]
derangementTests = [ Test "Valid Tests" testIsDerangement [
    ([1,3,5], [5,1,3], True),
    ([2,3,4,6,7,8,9], [9,8,7,4,6,2,3], True),
    ([0,1,5], [1,5,0], True),
    ([0,2,-2,2,0],[2,-2,0,0,2], True),
    ([-5,4,3,7],[3,-5,7,4], True),
    ([10,11,12,13,14],[14,12,11,10,13], True),
    ([8,4,2,1],[4,1,2,8], True),    
    ([45,32,87,54],[54,87,32,45], True)                                           ]
        ]

noDerangementTests :: [Test]
noDerangementTests = [ Test "Invalid Tests" testIsDerangement [
    ([1,3,5], [5,3,1], False),
    
    ([2,3,4,6,7,8,9], [9,8,7,4,6,2,3], False),
    ([0,1,5], [1,5,0], False),
    ([0,2,-2,2,0],[2,-2,0,0,2], False),
    ([-5,4,3,7],[3,-5,7,4], False),
    ([10,11,12,13,14],[14,12,11,10,13], False),
    ([8,4,2,1],[4,1,2,8], False),    
    ([45,32,87,54],[54,87,32,45], False)                        ]
                            
            ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ derangementTests
                  , noDerangementTests
                  ]

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

pick :: [[Int]] -> IO [Int]
pick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)
   
shuffle :: [Int] -> IO [Int]
shuffle xs = pick $ permutations xs
-- ^ shuffle list to see if a permutation could possibly be a derangement

testR :: Int -> Int -> ([Int] -> [Int] -> Bool) -> ([Int] -> [Int] -> Bool) -> IO ()
testR k n f r = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- genIntList
                  ys <- genIntList
                  if r xs ys == (f xs ys) then
                    do print ("pass on: " ++ show xs)
                       testR (k+1) n f r
                  else error ("failed test on: " ++ show xs)
                  
testDerangements :: Int -> Int -> ([Int] -> [Int] -> Bool) -> ([Int] -> [Int] -> Bool) -> IO()
testDerangements k n f r = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- genIntList
                  ys <- shuffle xs
                  if r xs ys  == (f xs ys) then
                    do print ("pass on: expected derangement?:" ++ show  (r xs ys || f xs ys) ++ " from: "++ show xs ++ " and " ++ show ys)
                       testDerangements (k+1) n f r
                  else error ("failed test on: " ++ show xs)
                  
samelength :: [Int] -> [Int] -> Bool
samelength xs ys = length xs == length ys

prop_ordered :: Ord a => [a] -> Bool
prop_ordered [] = True
prop_ordered (x:xs) = all (>= x) xs && prop_ordered xs

prop_samelength :: [a] -> [a] -> Bool
prop_samelength xs ys  = length xs == length ys