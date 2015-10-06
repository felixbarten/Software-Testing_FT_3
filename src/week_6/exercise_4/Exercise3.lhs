> module Exercise3 where



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


> composites :: [Integer]
> composites = filter (not.isPrime) [3..]