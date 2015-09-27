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

it's not a fast solution but it works

> project_euler_12 = filter ((>500) . getDivisors) $ map (triangularTerms) [1..] 


Euler problem 20 

** Fuctions below were implemented for ass#1 **

We run the follow :

> factorial n = if n < 2 then 1 else n * factorial (n-1)

> lastDigit :: Integer -> Integer
> lastDigit x = x`mod`10

> dropLastDigit :: Integer -> Integer
> dropLastDigit x = x `div` 10

> toRevDigits :: Integer -> [Integer]
> toRevDigits x
>        | x <= 0 = []
>        | otherwise = x `mod` 10 : toRevDigits (x `div` 10)


-- Calculate the sum of all the digits in every Integer.

> sumDigits :: [Integer] -> Integer
> sumDigits l = findSum 0 l
>        where
>          findSum n [] = n
>          findSum n (x:xs) =
>                if x < 10
>                 then findSum (n+x) xs
>                else findSum (n + lastDigit x + lastDigit (dropLastDigit x)) xs

> project_euler_20 = sumDigits $ toRevDigits $ factorial 100

        **** Outcome : 648 *******
        
Time spent: about 90 minutes on 7,8 and 12

        