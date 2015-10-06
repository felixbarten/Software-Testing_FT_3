> module Exercise3 where


Copied code from Lecture 6

> isPrime n = factors n == [n]
> primes = 2 : filter isPrime [3..]


> factors :: Integer -> [Integer]
> factors n = let 
>    ps = takeWhile (\m -> m^2 <= n) primes
>  in factors' n ps where 
>    factors' 1 _  = []
>    factors' n [] = [n]
>    factors' n (p:ps) 
>     | n `mod` p == 0 = p: factors' (n `div` p) (p:ps)
>     | otherwise      =    factors' n ps


Composites list:

> composites :: [Integer]
> composites = filter (not.isPrime) [1..]