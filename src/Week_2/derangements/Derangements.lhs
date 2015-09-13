> module Derangements where
 
> import Testing

Test Report
==========

For the testing we make use of the Testing module from Week_1 to test isDerangement and deran using values whose result is also already known. There are also
automated tests for the deran function which execute 100 times. They test the following properties of the post conditons of deran (n is provided as a parameter to deran): 


- The length of every list produced is equal to the value of n 
- The value of n is not an element of any list produces by deran 

*Time spent: 2 hours*

This function was taken from the workshop exercises

> permutations :: [a] ->[[a]]
> permutations [] = [[]]
> permutations (x:xs) = concatMap (insrt x) (permutations xs)
>                       where insrt x [] = [[x]]
>                             insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

This function is from the previous exercise 

> isPermutation :: Eq a => [a] -> [a] -> Bool
> isPermutation xs ys = all (`elem` ys) xs && all(`elem` xs) ys 

The function which checks if a list is a derangement of another

> isDerangement :: Eq a => [a] -> [a] -> Bool
> isDerangement xs ys = isPermutation xs ys  && helper xs ys
>                       where helper [] [] = True
>                             helper (x:xs) (y:ys) = x /= y && helper xs ys 

The function which generates all derangements based on the n provided 

> deran :: Int -> [[Int]]
> deran n = filter (isDerangement list) $ permutations list 
>           where list = [0..n-1]
> 

Tests using predefined values

> testIsDerangement :: Eq a => (Bool,[a],[a]) -> Bool
> testIsDerangement (r, xs, ys)  = isDerangement xs ys == r

> isDerangementTests :: [Test]
> isDerangementTests = [ Test "isDerangement Test" testIsDerangement [(True,[1,2,3], [3,1,2]), (False, [1,2,3], [1,2,4]), (False, [1,2], [3,4]), (True,[5,1,6,7], [7,6,5,1])]]

> testDeran :: (Int, [[Int]]) -> Bool
> testDeran (n, xss)  = deran n == xss

> deranTests :: [Test]
> deranTests =  [ Test "deran test" testDeran
>              [(0,[[]] ), (2, []), (3,[[2,1]]), (5,[[2,3,4,1],[3,1,4,2],[3,4,1,2],[3,4,2,1],[2,1,4,3],[2,4,1,3],[4,1,2,3],[4,3,1,2],[4,3,2,1]])]]

> allTests :: [Test]
> allTests = deranTests ++ isDerangementTests

Automated tests using random values and post condition properties

> testR :: Int -> Int -> (Int -> [[Int]])
>                     -> (Int -> [[Int]] -> Bool) -> IO ()
> testR k n f r = if k == n then print (show n ++ " tests passed")
>                 else do
>                   x <- getRandomInt 5
>                   let res = f x
>                   if r x res then
>                     do print ("pass on: " ++ show x ++ ", " ++ show res)
>                        testR (k+1) n f r
>                   else error ("failed test on: " ++ show x ++ ", " ++ show res)

Used to test post conditions (Based on the function provided by the lecturer) 

> testPost :: (Int -> [[Int]]) -> (Int -> [[Int]] -> Bool ) -> IO ()
> testPost = testR 1 100  

Used to check that all elements in the list have a length equal to n

> prop_lengthIsEqualTo :: Int -> [[Int]] -> Bool
> prop_lengthIsEqualTo n nss = all (\x -> length x == n) nss

This makes sure that n is a not a member of any of the lists generated

> prop_notElementOf :: Int -> [[Int]] -> Bool
> prop_notElementOf n nss = all (\x ->  n `notElem` x) nss

> testPostDeran1 :: IO ()
> testPostDeran1  = testPost deran prop_lengthIsEqualTo

> testPostDeran2 :: IO ()
> testPostDeran2  = testPost deran prop_notElementOf


> testPostDeran = do 
>                x <- testPostDeran1
>                y <- testPostDeran2
>                return (x,y)
