> module Exercise10 where


> fib :: Integer -> Integer
> fib 1 = 1
> fib 2 = 1
> fib n = fib (n-1) + fib (n-2)

> firstWithThreeDigits :: Integer -> Integer -> Integer
> firstWithThreeDigits x y |  ((fib x) `div` 100 >= 1) = y 
>			   | otherwise = let a = fib(x+1)
>					     b = y+1
>					 in firstWithThreeDigits a b
