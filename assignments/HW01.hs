{-# OPTIONS_GHC -Wall #-}
module HW01 where

import Debug.Trace

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits x = if x <= 0 then [] else  [lastDigit x] ++ toRevDigits(dropLastDigit x)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs) = x : (2* head xs) : doubleEveryOther(tail xs)

makeProducts :: [Integer] -> [Integer]
makeProducts [] = []
makeProducts (x:xs) = if x > 9 then (lastDigit x + dropLastDigit x):makeProducts xs else x: makeProducts xs
-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits x = foldr (+) 0 x

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn x = sumDigits(makeProducts(doubleEveryOther (toRevDigits x))) `mod` 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined
