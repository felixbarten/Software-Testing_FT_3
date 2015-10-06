> module Exercise1 where


> import Data.List
> import qualified Lecture6
> import Crypto.Number.ModArithmetic

Dependency on cryptonite lib.


Not that fast ;) 1.13 seconds with lecture code 0.72 with this code. 

> exM :: Integer -> Integer -> Integer -> Integer
> exM 0 0 z = 0
> exM x y z = rem (product ( map (\y' ->rem (x^y') z) $ getExponents y)) z

Even slower (without modding in between)

> exM' :: Integer -> Integer -> Integer -> Integer
> exM' 0 0 z = 0
> exM' x y z = rem (product ( map (\y' ->x^y') $ getExponents y)) z

A faster method would be to calculate x^y mod z with a power of 2 and then calculate the next one with precious results
x^4 mod z =  x^2 mod z * x^2 mod z

Decimal to Binary format, courtesy of stackoverflow

> decToBin x = reverse $ decToBin' x
>   where
>     decToBin' 0 = []
>     decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a

Convert back

> binToDec l = sum $ map (2^) $ findIndices (==1) $ reverse l

Convert y in powers of two

> getExponents :: Integer -> [Integer]
> getExponents exp = map (2^) $ findIndices (==1) $ reverse $ decToBin exp 




