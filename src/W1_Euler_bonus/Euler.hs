module W1_Euler_bonus.Euler where

import Data.List


--- Project Euler exercise 9

pythagoreanTriplet :: [(Integer, Integer, Integer)]
pythagoreanTriplet = [(a,  b,  c) | a <- [1..500], b <- [2..500], c <- [1..1000], a + b + c == 1000, a^2 + b^2 == c^2, a<b, b<c]
-- ^ will run for a loong time

productOf :: Integer
productOf = foldr (\(x,y,z) acc -> product [x,y,z] ) 0 pythagoreanTriplet

--- Project Euler exercise 10

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) [2..]
  
primes :: [Integer]
primes = filter prime [2..]

sumOfPrimes ::  Integer
sumOfPrimes = sum (takeWhile (< 2000000) primes)

-- Project Euler exercise 49

fourDigitPrimes :: [Integer]
fourDigitPrimes = dropWhile (<=1488) (takeWhile (<10000) primes)
-- ^ set lowerbound to 1488 to prevent returning (1487,4817,8147)

getPermutations :: [Integer] -> [Integer] -> (Integer,Integer,Integer)
getPermutations [] _ = (0,0,0)
getPermutations (x:xs) ys = if result == (0,0,0) then getPermutations xs ys else result 
    where result =  permutation x ys 
    
permutation :: Integer -> [Integer] -> (Integer, Integer, Integer)
permutation _ [] = (0,0,0)
permutation x (y:ys) = if x /= y && prime (y + (y - x)) && isPermutation x y (y + y - x)
  then (x, y, y + y - x) else permutation x ys

intToList :: Integer -> [Integer]
intToList = map (read . return) . show

{-
isPermutation :: Integer -> Integer -> Integer -> Bool
isPermutation x y z = (sort ( intToList x) == sort  (intToList y)) && (sort ( intToList y) == sort (intToList z)) && (sort ( intToList x) == sort ( intToList z))
-- ^ dirty implementation
-}

isPermutation :: Integer -> Integer -> Integer -> Bool
isPermutation x y z = intToList x `elem` ps && intToList y `elem` ps && intToList z `elem` ps
    where ps = permutations $ intToList x
-- ^ cleaner method

joinints :: [Integer] -> Integer
joinints = read . concatMap show

concatInts :: (Integer, Integer, Integer) -> Integer
concatInts (x, y, z) = joinints [x,y,z]

getPrimePermutations :: (Integer, Integer, Integer)
getPrimePermutations = getPermutations fourDigitPrimes fourDigitPrimes

getConcatPermutation :: Integer
getConcatPermutation = concatInts getPrimePermutations