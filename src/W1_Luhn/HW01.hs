{-# OPTIONS_GHC -Wall #-}
module W1_Luhn.HW01 where

-- Exercise 1 -----------------------------------------

digitToList :: Integer -> [Integer]
digitToList = map (read . return) . show

listToDigit :: [Integer] -> Integer
listToDigit = foldl addDigit 0
   where addDigit num d = 10*num + d

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit = last .digitToList 

-- Drop the last digit from a number 
dropLastDigit :: Integer -> Integer
dropLastDigit = listToDigit . reverse . drop 1 . reverse .digitToList

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits = reverse . digitToList

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = [] 
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = [x, y *2] ++ doubleEveryOther xs


-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits xs = foldr ((+) . sum . digitToList) 0 xs


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = rem (sumDigits . doubleEveryOther $ toRevDigits n) 10 == 0

{- A version of the algorithm I wrote before doing the Exercises is available in Luhn.hs -}
