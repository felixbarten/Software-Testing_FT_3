> module Exercise1 where

> import Data.List
> import Test.QuickCheck

*** Time spent : 2hrs ***

> checkExp ::  Integer -> Integer -> Integer -> Bool
> checkExp x y z = if z <= 0 then True
>			else exM x y z == expM x y z

> expM ::  Integer -> Integer -> Integer -> Integer
> expM x y = rem (x^y)

> exM :: Integer -> Integer -> Integer -> Integer
> exM x 1 z = rem x z
> exM x y z = rem (product $ map (\y -> modOfSquarePower x y z) ys) z
>	where ys = conToSquare (reverse $ decToBin y) 0


> decToBin :: Integer -> [Integer]
> decToBin x = reverse $ decToBin' x
>     where
>       decToBin' 0 = []
>       decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a


> conToSquare :: [Integer] -> Integer -> [Integer]
> conToSquare [] pos = []
> conToSquare (x:xs) pos |  x == 0 = conToSquare xs (pos+1)
>			| otherwise = 2^pos : conToSquare xs (pos+1)


> modOfSquarePower :: Integer -> Integer -> Integer -> Integer
> modOfSquarePower x 1 z = rem x z
> modOfSquarePower x y z = rem (square (modOfSquarePower x (y`div`2) z)) z


> square :: Integer -> Integer
> square x = x * x
