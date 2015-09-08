module Week_2.W2_IBAN.IBAN where

import Data.Char 
import Data.List

iban :: String -> Bool
iban nr = rem banknr 97 == 1 
    where banknr = (strToInteger . stepTwo . stepOne . trimSpaces) nr
    
trimSpaces      :: String -> String
trimSpaces [] = []
trimSpaces (c:cs) = if isSpace c then trimSpaces cs else c : trimSpaces cs

stepOne :: String -> String
stepOne s = drop 4 $ s ++ begin
    where begin = take 4 s
    
stepTwo :: String -> String
stepTwo [] = []
stepTwo [c] = convertAlphabetic c
stepTwo (c:cs) =  convertAlphabetic c ++ stepTwo cs

digits :: Int -> [Int]
digits = map (read . return) . show

strToInteger :: String -> Integer 
strToInteger s = read s :: Integer
-- ^ can cause errors

convertToChar :: Int -> String
convertToChar x = foldr (\x acc-> intToDigit x:acc) [] (digits x)

convertAlphabetic :: Char -> String
convertAlphabetic ch | ch == 'A' = convertToChar 10
                     | ch == 'B' = convertToChar 11
                     | ch == 'C' = convertToChar 12
                     | ch == 'D' = convertToChar 13
                     | ch == 'E' = convertToChar 14
                     | ch == 'F' = convertToChar 15
                     | ch == 'G' = convertToChar 16
                     | ch == 'H' = convertToChar 17
                     | ch == 'I' = convertToChar 18
                     | ch == 'J' = convertToChar 19
                     | ch == 'K' = convertToChar 20
                     | ch == 'L' = convertToChar 21
                     | ch == 'M' = convertToChar 22
                     | ch == 'N' = convertToChar 23
                     | ch == 'O' = convertToChar 24
                     | ch == 'P' = convertToChar 25
                     | ch == 'Q' = convertToChar 26
                     | ch == 'R' = convertToChar 27
                     | ch == 'S' = convertToChar 28
                     | ch == 'T' = convertToChar 29
                     | ch == 'U' = convertToChar 30
                     | ch == 'V' = convertToChar 31
                     | ch == 'W' = convertToChar 32
                     | ch == 'X' = convertToChar 33
                     | ch == 'Y' = convertToChar 34
                     | ch == 'Z' = convertToChar 35
                     | otherwise = [ch]
                    
                
