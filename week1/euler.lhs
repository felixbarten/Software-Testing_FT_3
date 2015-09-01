We use this convert an integer to a list of its digit

> import Data.Char
> import Data.List

Euler 9: Special Pythagorean triplet
===========================

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2
For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.


> -- x^2 + y^2 = z^2 
> pythagoreanTriples :: Integer -> [(Integer, Integer, Integer)]
> pythagoreanTriples n = [( x, y, z) | x <- [1..n-2], y <- [1..n-1], z <- [1..n], x < y && y < z, (x^2) + (y^2) == (z^2)]

Solution: 

> getSolution =  filter (\(x,y,z) -> x + y + z == 1000) $ pythagoreanTriples 500


Euler 10: Summation of primes

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
Find the sum of all the primes below two million.


> prime :: Integer -> Bool
> prime n = all (\ x -> rem n x /= 0) xs
>   where xs = takeWhile (\ y -> y^2 <= n) [2..]

> primes:: [Integer] 
> primes = filter prime [2..]

> getSumPrimes = sum (take (2000000-1) primes)
> getSumPrimes' n = sum [x | x <- [2..n], prime x]

Euler 49: Prime permutations
===========================
The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330,
is unusual in two ways: (i) each of the three terms are prime, and,
(ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property,
but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this sequence?

> digitsToList :: Integer -> [Integer]
> digitsToList = map (toInteger . digitToInt) . show

> fromDigits :: [Integer] -> Integer
> fromDigits = foldl addDigit 0
>   where addDigit num d = 10*num + d

> getAllPossiblePerm :: Integer -> [Integer]
> getAllPossiblePerm = nub . map fromDigits . permutations . digitsToList 

> isLongerThan2 :: [a] -> Bool
> isLongerThan2 xs = length xs > 2

> isPrimeT :: Integer -> (Integer, Bool)
> isPrimeT x =  (x, prime x)

> isPrimeTs :: [Integer] -> [(Integer, Bool)]  
> isPrimeTs  = map isPrimeT

> isLength3 :: [a] ->  Bool
> isLength3 xs =  length xs == 3

 filter (\x -> filter (isLength3) x) . map (isPrimeTs) . map getAllPossiblePerm








