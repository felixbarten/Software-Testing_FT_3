> module Exercise10 where

> import Data.List
> import Data.Char

Euler 7

> prime :: Integer -> Bool
> prime n = n > 1 && all (\ x -> rem n x /= 0) xs
>  where xs = takeWhile (\ y -> y^2 <= n) [2..]
  
> primes :: [Integer]
> primes = filter prime [2..]

> project_euler_7 :: Integer
> project_euler_7 = last $ take 10001 primes


Euler 8

> digits :: Int -> [Int]
> digits = map (read . return) . show


Create int list from file. 

> thousandDigitLst = do
>   str <- readFile "digit.txt"
>   return $ map (fromIntegral . digitToInt)  . concat . lines $ str


> project_euler_8 =  do 
>   list <- thousandDigitLst
>   print . maximum . map product . foldr (zipWith (:)) (repeat []) $ take 13 . tails $ list
         
Euler 9 & 10 done in week one. 

skip Euler 11...

Euler 12

> triangularTerms :: Integer -> Integer
> triangularTerms n = sum [0..n]


> getDivisors :: Integer -> Int
> getDivisors n = length . filter (\x -> rem n x == 0) $[1..n]

> project_euler_12 = filter ((>500) . getDivisors) $ map (triangularTerms) [1..] 



> primes' = 2 : filter (null . tail . primeFactors) [3,5..]
 
> primeFactors n = factor n primes'
>   where
>     factor n (p:ps) 
>         | p*p > n        = [n]
>         | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
>         | otherwise      =     factor n ps
>  
> problem_3 = last (primeFactors 600851475143)

> problem_12 = head $ filter ((> 500) . nDivisors) triangleNumbers
>  where nDivisors n = product $ map ((+1) . length) (group (primeFactors n))    
>        triangleNumbers = scanl1 (+) [1..]


foldr (\x acc -> if rem n x == 0 then x : acc else (acc)) [] [1..n]