> module Exercise5 where

> import Lecture6


Report
======

The fermat test is likely to fail regardless of the size of k in this case because carmichael numbers share the same
properties as prime numbers which tests for.


> findCE :: Int -> IO Integer
> findCE n = do
>            x <- prime_test_F (carmichael !! n)
>            print x  
>            print (carmichael !! n)
>            if x then return (carmichael !! n) else findCE (n+1) 

> findCE' :: Int -> Int -> IO Integer
> findCE' k n = do
>            x <- prime_tests_F k (carmichael !! n)
>            print $ (show x)  ++  " " ++ (show  $ carmichael !! n) ++  " " ++ (show k) 
>            if x then findCE' (k+1) (n) else  findCE' (1) (n+1)

> carmichael :: [Integer]
> carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
>       k <- [2..], 
>       isPrime (6*k+1), 
>       isPrime (12*k+1), 
>       isPrime (18*k+1) ]
