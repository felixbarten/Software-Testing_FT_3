module Luhn where

{- 
Eigen implementatie van Luhn mod 10 

-}

digits :: Integer -> [Integer]
digits = map (read . return) . show

integerToList ::  Integer -> [Integer]
integerToList x = digits x

evenNumbers :: [Integer] -> [Integer]
evenNumbers [] = [] 
evenNumbers [_] = []
evenNumbers (_:y:xs) = y : evenNumbers xs
-- ^ appends the numbers found in even places in the list

oddNumbers :: [Integer] -> [Integer]
oddNumbers [] = [] 
oddNumbers (x:xs) = if length xs >= 1 then  x : oddNumbers (drop 1 xs) else [x]
-- ^ possible errors with dropping elements from the list should be avoided with the length call. 

calculateChecksum :: Integer -> Integer
calculateChecksum n = sum ((oddNumbers . reverse . integerToList) n) + sumEvenNum (evenNumbers . reverse . integerToList $ n)

sumEvenNum :: [Integer] -> Integer
sumEvenNum xs = foldr (\ x -> (+) (sum (integerToList (x * 2)))) 0 xs

luhnValid :: Integer -> Bool
luhnValid n = rem (calculateChecksum n) 10 == 0

