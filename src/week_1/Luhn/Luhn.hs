module W1_Luhn.Luhn where

{- 
Eigen implementatie van Luhn mod 10 

-}

digits :: Integer -> [Integer]
digits = map (read . return) . show

integerToList ::  Integer -> [Integer]
integerToList = digits

evenNumbers :: [Integer] -> [Integer]
evenNumbers [] = [] 
evenNumbers [_] = []
evenNumbers (_:y:xs) = y : evenNumbers xs
-- ^ appends the numbers found in even places in the list

oddNumbers :: [Integer] -> [Integer]
oddNumbers [] = [] 
oddNumbers (x:xs) = if not (null xs) then  x : oddNumbers (drop 1 xs) else [x]
-- ^ possible errors with dropping elements from the list should be avoided with the length call. 

calculateChecksum :: Integer -> Integer
calculateChecksum n = sum ((oddNumbers . reverse . integerToList) n) + sumEvenNum (evenNumbers . reverse . integerToList $ n)

sumEvenNum :: [Integer] -> Integer
sumEvenNum = foldr (\ x -> (+) (sum (integerToList (x * 2)))) 0

luhnValid :: Integer -> Bool
luhnValid n = rem (calculateChecksum n) 10 == 0

