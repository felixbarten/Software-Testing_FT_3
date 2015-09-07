module Week_2.W2_IBAN.IBAN where

import Data.Char 

iban :: String -> Bool
iban = undefined

stepOne :: String -> String
stepOne s = drop 4 $ s ++ begin
    where begin = take 4 s
    
stepTwo :: String -> String
stepTwo [] = []
stepTwo [c] = [convertAlphabetic c]
stepTwo (c:cs) = convertAlphabetic c : stepTwo cs

convertAlphabetic :: Char -> Char
convertAlphabetic ch | ch == 'A' = chr 10
                     | ch == 'B' = chr 11
                     | ch == 'C' = chr 12
                     | ch == 'D' = chr 13
                     | ch == 'E' = chr 14
                     | ch == 'F' = chr 15
                     | ch == 'G' = chr 16
                     | ch == 'H' = chr 17
                     | ch == 'I' = chr 18
                     | ch == 'J' = chr 19
                     | ch == 'K' = chr 20
                     | ch == 'L' = chr 21
                     | ch == 'M' = chr 22
                     | ch == 'N' = chr 23
                     | ch == 'O' = chr 24
                     | ch == 'P' = chr 25
                     | ch == 'Q' = chr 26
                     | ch == 'R' = chr 27
                     | ch == 'S' = chr 28
                     | ch == 'T' = chr 29
                     | ch == 'U' = chr 30
                     | ch == 'V' = chr 31
                     | ch == 'W' = chr 32
                     | ch == 'X' = chr 33
                     | ch == 'Y' = chr 34
                     | ch == 'Z' = chr 35
                     | otherwise = ch
                    
                
