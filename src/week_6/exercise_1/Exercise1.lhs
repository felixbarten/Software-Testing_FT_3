> module Exercise1 where


> import Data.List
> import Lecture6



> exM :: Integer -> Integer -> Integer -> Integer
> exM 0 0 z = 0
> exM x y z = rem (product ( map (x^) $ getExponents y)) z


Decimal to Binary format, courtesy of stackoverflow

> decToBin x = reverse $ decToBin' x
>   where
>     decToBin' 0 = []
>     decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a

Convert back

> binToDec l = sum $ map (2^) $ findIndices (==1) $ reverse l


> getExponents :: Integer -> [Integer]
> getExponents exp = map (2^) $ findIndices (==1) $ reverse $ decToBin exp 

