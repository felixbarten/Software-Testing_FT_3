> {-# OPTIONS_GHC -Wall #-}
> module HW01 where
> import Data.Char

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number

> lastDigit :: Integer -> Integer
> lastDigit n = rem n 10 

-- Drop the last digit from a number

> dropLastDigit :: Integer -> Integer
> dropLastDigit n = div n 10

-- Exercise 2 -----------------------------------------

> toRevDigits :: Integer -> [Integer]
> toRevDigits  = reverse . map (toInteger . digitToInt) . show  

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.

> doubleEveryOther :: [Integer] -> [Integer]
> doubleEveryOther [] = []
> doubleEveryOther [x] = [x]
> doubleEveryOther (x:y:rest) = x:y* 2 : doubleEveryOther rest

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.

> sumDigits :: [Integer] -> Integer
> sumDigits = foldr (+) 0 

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.

> luhn :: Integer -> Bool
> luhn x = mod  (aux x) 10  == 0
>      where aux = sumDigits . map (\n -> if n > 9 then dropLastDigit n + lastDigit n else n) .  doubleEveryOther . toRevDigits

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
 
> type Peg = String
> type Move = (Peg, Peg)

> hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
> hanoi = undefined
