> module Exercise10 where

** Fuctions below were implemented for ass#1 **

We run the follow :

*Exercise10> sumDigits $ toRevDigits $ factorial 100

	**** Outcome : 648 *******

> factorial n = if n < 2 then 1 else n * factorial (n-1)

> lastDigit :: Integer -> Integer
> lastDigit x = x`mod`10

> dropLastDigit :: Integer -> Integer
> dropLastDigit x = x `div` 10

> toRevDigits :: Integer -> [Integer]
> toRevDigits x
>	| x <= 0 = []
>	| otherwise = x `mod` 10 : toRevDigits (x `div` 10)


-- Calculate the sum of all the digits in every Integer.

> sumDigits :: [Integer] -> Integer
> sumDigits l = findSum 0 l
>	where
>	  findSum n [] = n
>	  findSum n (x:xs) =
>		if x < 10
>		 then findSum (n+x) xs
>		else findSum (n + lastDigit x + lastDigit (dropLastDigit x)) xs
