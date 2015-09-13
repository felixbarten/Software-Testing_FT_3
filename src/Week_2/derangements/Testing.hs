{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}
module Testing where

import Data.Maybe
import Control.Arrow

--Cabal pls
import Data.List
import System.Random

data Test    = forall a. Show a => Test String (a -> Bool) [a]
data Failure = forall a. Show a => Fail String [a]

instance Show Failure where
    show (Fail s as) = "Failed Test \"" ++ s
                       ++ "\" on inputs " ++ show as

runTest :: Test -> Maybe Failure
runTest (Test s f as) = case filter (not . f) as of
                          [] -> Nothing
                          fs -> Just $ Fail s fs

runTests :: [Test] -> [Failure]
runTests = catMaybes . map runTest

-- Helpers

testF1 :: (Show a, Show b, Eq b) => String -> (a -> b) -> [(a, b)] -> Test
testF1 s f l = Test s (uncurry (==)) $ map (first f) l

testF2 :: (Show a, Show b, Show c, Eq c) => String -> (a -> b -> c)
       -> [(a, b, c)] -> Test
testF2 s f l = Test s (uncurry (==)) $ map (\(x, y, z) -> (f x y, z))  l

testF3 :: (Show a, Show b, Show c, Show d, Eq d) => String -> (a -> b -> c -> d)
       -> [(a, b, c, d)] -> Test
testF3 s f l = Test s (uncurry (==)) $ map (\(w, x, y, z) ->  (f w x y, z)) l

------- Misc

-- Generate a random integer
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

-- Always returns true
isTrue :: a -> Bool
isTrue _ = True

-- Can be used to test the precondition
hasDuplicates :: Eq a => [a] -> Bool 
hasDuplicates xs = length xs /= length  (nub xs) 

-- Used to generate even more random stuff
coinToss ::IO Int  
coinToss = getRandomInt 1

randomFlip :: Int -> IO Int
randomFlip x = do 
   b <- getRandomInt 1
   if b == 0 then return x else return (-x)

-- Generate random list of integers
genIntList' :: IO [Int]
genIntList' = do 
  k <- getRandomInt 20
  n <- getRandomInt 5
  getIntL k n

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)

-- Generate a pseudorandom list (with no dups) and its permutation then maybe it'll generate a new list just to mess with everyone's head
getListPermutations :: IO ([Int],[[Int]])
getListPermutations = do 
   x <-  genIntList'
   c <- coinToss
   let m =  nub x
   if c == 1 then do
    y <- genIntList' 
    return (nub y, permutations m)
   else return (m, permutations m) 

-- Run dem tests
testR' :: Int -> Int -> ([Int] -> [Int] -> Bool)
                    -> ([Int] -> [Int] -> Bool) -> IO ()
testR' k n f r = if k == n then print (show n ++ " tests passed")
                else do
                  (xs, xss) <- getListPermutations
                  l <- getRandomInt (length xss -1 ) 
                  let ys =  xss !! l
                  if r xs ys == f xs ys then
                    do print ("Expected result: " ++ show (r xs ys) ++ "  => pass on: " ++ show xs ++ ", " ++ show ys)
                       testR' (k+1) n f r
                  else error ("Expected result: " ++ show (r xs ys) ++ " => failed test on: " ++ show xs ++ ", " ++ show ys)

-- Used to test post conditions  
testPost' :: ([Int] -> [Int]-> Bool) -> ([Int] -> [Int] -> Bool ) -> IO ()
testPost' = testR' 1 100  
