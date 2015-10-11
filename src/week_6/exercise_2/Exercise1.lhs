> module Exercise1 where
> import Data.Bits

> exM :: Integer -> Integer -> Integer -> Integer
> exM b 0 m = 1
> exM b e m = t * exM ((b^2) `mod` m) (e `shiftR` 1) m `mod` m
>  		   where t = if e `mod` 2 == 1 then b `mod` m else 1
  

> expM ::  Integer -> Integer -> Integer -> Integer
> expM x y = rem (x^y)

