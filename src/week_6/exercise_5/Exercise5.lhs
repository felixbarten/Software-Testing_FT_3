> module Exercise5 where

> import Exercise3
> import Lecture6
> import Control.Monad

> carmichael :: [Integer]
> carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
>       k <- [2..], 
>       isPrime (6*k+1), 
>       isPrime (12*k+1), 
>       isPrime (18*k+1) ]


> divide :: Integer -> Integer -> Bool
> divide n m = rem m n == 0


> testFermat k xs= filterM (\c -> tstFer k c) xs

 
> tstFer k n = do
>                result <- prime_tests_F k n
>                when (result) $ print ("True for composite " ++ show n ++ " and K: " ++ show k) 
>                return result
