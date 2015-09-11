module Week_2.W2_Euler_bonus.Euler where

import Data.List

-- Euler 2

fibonacci_seq :: [Integer]
fibonacci_seq = 1: 1 : zipWith (+) fibonacci_seq (tail fibonacci_seq)


project_euler_2 :: Integer 
project_euler_2 = sum [ x | x <- takeWhile (<= 4000000) fibonacci_seq, even x]

-- Euler 3 

prime:: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) [2..]
  
primes:: [Integer]
primes = filter prime [2..]

primeFactors :: Integer -> [Integer]
primeFactors n = filter (\x -> rem n x == 0) (takeWhile (\p -> p*p<n) primes)

project_euler_3 :: Integer
project_euler_3 = last $ primeFactors 600851475143  

-- Euler 4

isPalindrome :: Integer -> Bool
isPalindrome x = x == (read . reverse . show $ x)

project_euler_4 :: Integer
project_euler_4 = maximum [ z | x <- [100..999], y <- [x..999], let z = x*y, isPalindrome z ] 
