> module Exercise6 where

> import Lecture6


Report 
======

The MR test is accurate when it comes to composite numbers but  it can also be fooled by the carmichael numbers. Although, it is much more accurate when than the 
Fermat test.


> carmichael :: [Integer]
> carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
>       k <- [2..], 
>       isPrime (6*k+1), 
>       isPrime (12*k+1), 
>       isPrime (18*k+1) ]

> testMRComposites :: Int -> Int -> IO Integer
> testMRComposites k n = do
>            x <- primeMR k y
>            print $  "Attempting to classify as (not) prime " ++ (show y) ++  " " ++  "Result (isPrime):  "++ (show x)++  " " ++  "k:  " ++ (show k) 
>            if x then testMRComposites (k+1) (n) else  testMRComposites (1) (n+1)
>            where y  = composites !! n

> testMRCarMichaels :: Int -> Int -> IO Integer
> testMRCarMichaels k n = do
>            x <- primeMR k y
>            print $  "Attempting to classify as (not) prime " ++ (show y) ++  " " ++  "Result (isPrime):  "++ (show x)++  " " ++  "k:  " ++ (show k) 
>            if x then testMRCarMichaels (k+1) (n) else  testMRCarMichaels (1) (n+1)
>            where y  = carmichael !! n


