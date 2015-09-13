> module Week_2.W2_IBAN.IBAN where

> import Data.Char (isAlphaNum,isDigit, digitToInt)
> import Testing

Test Report
==========

There are automated tests for the following properties of functions used to compose the validateIban function.

- convertLetterToNum : prop_isTwoDigitNumber
- moveFirstFourToBack : prop_samelength



*Time spent: 3 hours*


> digits :: Int -> [Int]
> digits = map (read . return) . show
 
> intToList ::  Int -> [Int]
> intToList = digits 
 
> removeNonAlphaNumeric :: String -> String
> removeNonAlphaNumeric = filter isAlphaNum
   
> moveFirstFourToBack :: String -> String
> moveFirstFourToBack s = drop 4 s ++ take 4 s
 
> convertLetterToNum :: Char -> Int
> convertLetterToNum c  |  c == 'A' = 10
>                       |  c == 'G' = 16
>                       |  c == 'M' = 22
>                       |  c == 'S' = 28
>                       |  c == 'Y' = 34
>                       |  c == 'B' = 11
>                       |  c == 'H' = 17
>                       |  c == 'N' = 23
>                       |  c == 'T' = 29
>                       |  c == 'Z' = 35
>                       |  c == 'C' = 12
>                       |  c == 'I' = 18
>                       |  c == 'O' = 24
>                       |  c == 'U' = 30
>                       |  c == 'D' = 13
>                       |  c == 'J' = 19
>                       |  c == 'P' = 25
>                       |  c == 'V' = 31
>                       |  c == 'E' = 14
>                       |  c == 'K' = 20
>                       |  c == 'Q' = 26
>                       |  c == 'W' = 32
>                       |  c == 'F' = 15
>                       |  c == 'L' = 21
>                       |  c == 'R' = 27
>                       |  c == 'X' = 33
>                       |  otherwise = error "Not a valid char"

> validChars :: [Char]
> validChars = ['A', 'G', 'M', 'S', 'Y', 'B', 'H', 'N', 'T', 'Z', 'C', 'I', 'O', 'U', 'D', 'J', 'P', 'V', 'E', 'K', 'Q', 'W', 'F', 'L', 'R', 'X']

> getRandomValidChar :: IO Char
> getRandomValidChar = do 
>                      x <- getRandomInt ((length validChars) - 1)
>                      return (validChars !! x)

> genRandomString :: Int ->  IO [Char]  
> genRandomString 0 = return []
> genRandomString n = do 
>    x <-  getRandomValidChar
>    xs <- genRandomString (n-1)
>    return (x:xs) 

> prop_betweenTwoDigitNumber :: Int -> Bool
> prop_betweenTwoDigitNumber n = n > 9 && n < 100


> toDigits :: String -> [Int]
> toDigits [] = []
> toDigits (c:cs) = if isDigit c then digitToInt c:toDigits cs else intToList (convertLetterToNum c) ++  toDigits cs
 
> fromDigits :: [Int] -> Integer
> fromDigits = foldl addDigit 0
>    where addDigit num d = 10 *  (toInteger num) + (toInteger d)
 
> mod9710 :: Integer -> Bool
> mod9710 x = n == 1
>           where n = mod x 97

Only works with a checkdigit

> validateIban :: String -> Bool
> validateIban =  mod9710 . toInteger . fromDigits . toDigits . moveFirstFourToBack . removeNonAlphaNumeric

Properties used to test

> prop_isTwoDigitNumber :: Int -> Bool
> prop_isTwoDigitNumber n = n > 9 && n < 100

> prop_samelength :: [a] -> [a] -> Bool
> prop_samelength x y = length y == length x

Automated tests using random values and post condition properties


> testRange :: Int -> Int -> (Char -> Int)
>                     -> (Int ->  Bool) -> IO ()
> testRange k n f r = if k == n then print (show n ++ " tests passed")
>                 else do
>                   x <- getRandomValidChar 
>                   let res = f x
>                   if r res then
>                     do print ("pass on: " ++ show x ++ ", " ++ show res)
>                        testRange (k+1) n f r
>                   else error ("failed test on: " ++ show x ++ ", " ++ show res)

> testLists :: Int -> Int -> ([Char] -> [Char])
>                     -> ([Char] -> [Char] -> Bool) -> IO ()
> testLists k n f r = if k == n then print (show n ++ " tests passed")
>                 else do
>                   y <- getRandomInt 10
>                   x <- genRandomString y
>                   let res = f x
>                   if r x res then
>                     do print ("pass on: " ++ show x ++ ", " ++ show res)
>                        testLists (k+1) n f r
>                   else error ("failed test on: " ++ show x ++ ", " ++ show res)

Used to test post conditions (Based on the function provided by the lecturer) 

> testConvertAlphaToNum :: IO ()
> testConvertAlphaToNum = testRange 1 100 convertLetterToNum prop_isTwoDigitNumber

> testMoveFirstFourToBack :: IO ()
> testMoveFirstFourToBack = testLists 1 100 moveFirstFourToBack prop_samelength
 
