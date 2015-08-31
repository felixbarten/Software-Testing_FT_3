module Luhn where

{- 
Eigen implementatie van Luhn mod 10 

-}

digits :: Integer -> [Integer]
digits = map (read . return) . show

takeDigits ::  Integer -> [Integer]
takeDigits x = digits x

{-
evenNumbers :: Integer -> [Integer]
evenNumbers x = takeDigits x 
-- ^ fout
-}

evenNumbers :: [Integer] -> [Integer]
evenNumbers [] = [] 
evenNumbers [_] = []
evenNumbers (_:y:xs) = y : evenNumbers xs

oddNumbers :: [Integer] -> [Integer]
oddNumbers [] = [] 
oddNumbers (x:xs) = if length xs >= 1 then  x : oddNumbers (drop 1 xs) else [x]
-- ^ possible errors?

calculateChecksum :: Integer -> Integer
calculateChecksum n = sum (oddNumbers $ reverse $ takeDigits n) + sumEvenNum (evenNumbers $ reverse $ takeDigits $ n)

sumEvenNum :: [Integer] -> Integer
sumEvenNum [] = 0
sumEvenNum (x:xs) = sum (takeDigits ( x * 2)) + sumEvenNum xs

luhnValid :: Integer -> Bool
luhnValid n = rem (calculateChecksum n) 10 == 0

