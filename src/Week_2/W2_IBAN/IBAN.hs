module Week_2.W2_IBAN.IBAN where

import Data.Char (isAlphaNum,isDigit, digitToInt)

digits :: Integer -> [Integer]
digits = map (read . return) . show

integerToList ::  Integer -> [Integer]
integerToList = digits 

removeNonAlphaNumeric :: String -> String
removeNonAlphaNumeric = filter isAlphaNum
  
moveFirstFourToBack :: String -> String
moveFirstFourToBack s = drop 4 s ++ take 4 s

convertLetterToNum :: Char -> Integer
convertLetterToNum 'A' = 10
convertLetterToNum 'G' = 16
convertLetterToNum 'M' = 22
convertLetterToNum 'S' = 28
convertLetterToNum 'Y' = 34
convertLetterToNum 'B' = 11
convertLetterToNum 'H' = 17
convertLetterToNum 'N' = 23
convertLetterToNum 'T' = 29
convertLetterToNum 'Z' = 35
convertLetterToNum 'C' = 12
convertLetterToNum 'I' = 18
convertLetterToNum 'O' = 24
convertLetterToNum 'U' = 30
convertLetterToNum 'D' = 13
convertLetterToNum 'J' = 19
convertLetterToNum 'P' = 25
convertLetterToNum 'V' = 31
convertLetterToNum 'E' = 14
convertLetterToNum 'K' = 20
convertLetterToNum 'Q' = 26
convertLetterToNum 'W' = 32
convertLetterToNum 'F' = 15
convertLetterToNum 'L' = 21
convertLetterToNum 'R' = 27
convertLetterToNum 'X' = 33
convertLetterToNum _ = -1

toDigits :: String -> [Integer]
toDigits [] = []
toDigits (c:cs) = if isDigit c then toInteger (digitToInt c):toDigits cs else integerToList (convertLetterToNum c) ++  toDigits cs

fromDigits :: [Integer] -> Integer
fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d

mod9710 :: Integer -> Bool
mod9710 x = n == 1
          where n = mod x 97
validateIban :: String -> Bool
validateIban =  mod9710 . fromDigits . toDigits . moveFirstFourToBack . removeNonAlphaNumeric

