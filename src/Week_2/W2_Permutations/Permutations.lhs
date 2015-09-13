> module Week_2.W2_Permutations.Permutations (isPermutation) where

> import Data.List
> import Data.Char
> import System.Random

Test Report
==========

Using the techniques shown during a lecture an automated test was created that generates a random list of integers and then generates a lists containing
all of its permutations. A coin is then flipped to decide whether or not to generate a new list. The list is then returned along with the permutations.
The permutations function from the Data.List module is used as a control function to validate my implementation of isPermutation. 
The function being tested is isPermutations. 

*Time spent: 3 hours*

A sorting function stolen from the lecture

> quicksort :: Ord a => [a] -> [a]  
> quicksort [] = []  
> quicksort (x:xs) = 
>    quicksort [ a | a <- xs, a <= x ]  
>    ++ [x]
>    ++ quicksort [ a | a <- xs, a > x ]

Checks is two lists are permutations

> isPermutation :: Ord a => [a] -> [a] -> Bool
> isPermutation xs ys =  aux  xs == aux ys 
>                   where aux = quicksort . nub

This is used to generate a random integer

> getRandomInt :: Int -> IO Int
> getRandomInt n = getStdRandom (randomR (0,n))

Always returns true

> isTrue :: a -> Bool
> isTrue _ = True

Can be used to test the precondition

> hasDuplicates :: Eq a => [a] -> Bool 
> hasDuplicates xs = length xs /= length  (nub xs) 

This function is used to flip a coin
  
> coinToss ::IO Int  
> coinToss = getRandomInt 1

Used to get a even distribution of positive and negative numbers
> randomFlip :: Int -> IO Int
> randomFlip x = do 
>    b <- getRandomInt 1
>    if b == 0 then return x else return (-x)

Generate random list of integers

> genIntList' :: IO [Int]
> genIntList' = do 
>   k <- getRandomInt 20
>   n <- getRandomInt 5
>   getIntL k n

> getIntL :: Int -> Int -> IO [Int]
> getIntL _ 0 = return []
> getIntL k n = do 
>    x <-  getRandomInt k
>    y <- randomFlip x
>    xs <- getIntL k (n-1)
>    return (y:xs)

Generate a pseudorandom list (with no dups) and its permutation then maybe it'll generate a new list just to mess with everyone's head

> getListPermutations :: IO ([Int],[[Int]])
> getListPermutations = do 
>    x <-  genIntList'
>    c <- coinToss
>    let m =  nub x
>    if c == 1 then do
>     y <- genIntList' 
>     return (nub y, permutations m)
>    else return (m, permutations m) 

Run dem tests

> testR' :: Int -> Int -> ([Int] -> [Int] -> Bool)
>                     -> ([Int] -> [Int] -> Bool) -> IO ()
> testR' k n f r = if k == n then print (show n ++ " tests passed")
>                 else do
>                   (xs, xss) <- getListPermutations
>                   l <- getRandomInt (length xss -1 ) 
>                   let ys =  xss !! l
>                   if r xs ys == f xs ys then
>                     do print ("Expected result: " ++ show (r xs ys) ++ "  => pass on: " ++ show xs ++ ", " ++ show ys)
>                        testR' (k+1) n f r
>                   else error ("Expected result: " ++ show (r xs ys) ++ " => failed test on: " ++ show xs ++ ", " ++ show ys)

Used to test post conditions  

> testPost' :: ([Int] -> [Int]-> Bool) -> ([Int] -> [Int] -> Bool ) -> IO ()
> testPost' = testR' 1 100  

Can be used to compare the length of two lists

> samelength :: [a] -> [a] -> Bool
> samelength xs ys = length xs == length ys


Used as a control function for testing the permutation function

> prop_perm :: Eq a => [a] -> [a] -> Bool
> prop_perm = isPerm   
>               where isPerm xs ys = all (`elem` ys) xs && all(`elem` xs) ys

Used as a control function for testing the permutation function

> prop_perm' :: Eq a => [a] -> [a] -> Bool
> prop_perm' xs ys = xs `elem` permutations ys

> testRel' :: ([Int] -> [Int] -> Bool) -> ([Int] -> [Int] -> Bool) -> IO ()
> testRel' = testR' 1 100 

Used to test the permutation function

> testIsPropPerm :: IO ()
> testIsPropPerm = testPost' isPermutation prop_perm'   



