> module Exercise5 where

> carmichael :: [Integer]
> carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
>       k <- [2..], 
>       isPrime (6*k+1), 
>       isPrime (12*k+1), 
>       isPrime (18*k+1) ]


> isPrime :: Integer -> Bool
> isPrime n = n > 1 && all (\ d -> not (divide d n)) [2..n-1]

> divide :: Integer -> Integer -> Bool
> divide n m = rem m n == 0
