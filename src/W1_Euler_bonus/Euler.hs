module W1_Euler_bonus.Euler where

import Data.List


--- Project Euler exercise 9

{- 
first (SLOW) solution: 
pythagoreanTriplet :: [(Integer, Integer, Integer)]
pythagoreanTriplet = [(a,  b,  c) | a <- [1..500], b <- [2..500], c <- [1..1000], a + b + c == 1000, a^2 + b^2 == c^2, a<b && b<c]
-}

{-
pythagoreanTriplet :: [[Integer]]
pythagoreanTriplet =  [[a,  b,  c] | j <- [2..500], i <- [1..(j-1)], let a = i, let b = j, let c = a + b, a^2 + b^2 == c^2]
-}

pythagoreanTriplet :: [(Integer, Integer, Integer)] 
pythagoreanTriplet =  [(a,  b,  c) | j <- [2..500],
                                     i <- [1..(j-1)],
                                      let a = i,
                                      let b = j, 
                                      let c = 1000 - (a + b), 
                                      a + b + c == 1000,
                                      a^2 + b^2 == c^2]

euler_problem_9 :: Integer
euler_problem_9 = foldr (\(x,y,z) _ -> product [x,y,z] ) 0 pythagoreanTriplet


--- Project Euler exercise 10

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) [2..]
  
primes :: [Integer]
primes = filter prime [2..]

euler_problem_10 ::  Integer
euler_problem_10 = sum (takeWhile (< 2000000) primes)

-- Project Euler exercise 49

fourDigitPrimes :: [Integer]
fourDigitPrimes = dropWhile (<=1488) (takeWhile (<10000) primes)
-- ^ set lowerbound to 1488 to prevent returning (1487,4817,8147)

getPrimeTriples :: [Integer] -> [Integer] -> (Integer,Integer,Integer)
getPrimeTriples [] _ = (0,0,0)
getPrimeTriples (x:xs) ys = if result == (0,0,0) then getPrimeTriples xs ys else result 
    where result =  getPrimeTriple x ys 
    
getPrimeTriple :: Integer -> [Integer] -> (Integer, Integer, Integer)
getPrimeTriple _ [] = (0,0,0)
getPrimeTriple x (y:ys) = if x /= y && prime z && isPermutation x y z
  then (x, y, z) else getPrimeTriple x ys
  where z = y + (y-x)

intToList :: Integer -> [Integer]
intToList = map (read . return) . show

{-
isPermutation :: Integer -> Integer -> Integer -> Bool
isPermutation x y z = (sort ( intToList x) == sort  (intToList y)) && (sort ( intToList y) == sort (intToList z)) && (sort ( intToList x) == sort ( intToList z))
-- ^ without permutations from data.list
-}

isPermutation :: Integer -> Integer -> Integer -> Bool
isPermutation x y z = intToList x `elem` ps && intToList y `elem` ps && intToList z `elem` ps
    where ps = permutations $ intToList x
-- ^ cleaner method

joinints :: [Integer] -> Integer
joinints = read . concatMap show

concatInts :: (Integer, Integer, Integer) -> Integer
concatInts (x, y, z) = joinints [x,y,z]

euler_problem_49 :: Integer
euler_problem_49 = concatInts $ getPrimeTriples fourDigitPrimes fourDigitPrimes
