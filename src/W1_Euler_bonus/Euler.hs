module W1_Euler_bonus.Euler where



--- Project Euler exercise 9




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
fourDigitPrimes = dropWhile (<999) (takeWhile (<10000) primes)
