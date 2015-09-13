{-# LANGUAGE DoAndIfThenElse #-}
module Week_2.W2_IBAN.IBAN where

import Data.Char 
import Data.List
import System.Random
import Week_2.Testing

iban :: String -> Bool
iban nr = rem banknr 97 == 1 
    where banknr = (strToInteger . convertAlphaNumeric . moveCheckDigits . trimSpaces) nr
    
trimSpaces      :: String -> String
trimSpaces [] = []
trimSpaces (c:cs) = if isSpace c then trimSpaces cs else c : trimSpaces cs

moveCheckDigits :: String -> String
moveCheckDigits s = drop 4 $ s ++ begin
    where begin = take 4 s
    
convertAlphaNumeric :: String -> String
convertAlphaNumeric [] = []
convertAlphaNumeric [c] = convertAlphabetic c
convertAlphaNumeric (c:cs) =  convertAlphabetic c ++ convertAlphaNumeric cs

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
                    
        
-- calculate check digits 
calculateCheckDigits :: String -> String
calculateCheckDigits nr = if (98 - rem iban 97 < 10) then "0" ++ show (98 - rem iban 97) else show (98 - rem iban 97)
    where iban = (strToInteger . convertAlphaNumeric. moveCheckDigits .  trimSpaces) nr

          
-- general not automated tests

testIban :: (String, Bool) -> Bool
testIban (s, b) = iban s == b


validIBANTests :: [Test]
validIBANTests = [ Test "Valid Tests" testIban [
    ("AL47 2121 1009 0000 0002 3569 8741", True),
    ("AD12 0001 2030 2003 5910 0100", True),
    ("FR14 2004 1010 0505 0001 3M02 606", True),
    ("HU42 1177 3016 1111 1018 0000 0000", True),
    ("JO94 CBJO 0010 0000 0000 0131 0003 02", True),
    ("MT84 MALT 0110 0001 2345 MTLC AST0 01S", True),
    ("QA58 DOHB 0000 1234 5678 90AB CDEF G", True),
    ("NL39 RABO 0300 0652 64", True),
    ("NO93 8601 1117 947", True),
    ("RO49 AAAA 1B31 0075 9384 0000", True),
    ("MU17 BOMM 0101 1010 3030 0200 000M UR", True),
    ("KW81 CBKU 0000 0000 0000 1234 5601 01", True),
    ("MK072 5012 0000 0589 84", True),
    ("TR33 0006 1005 1978 6457 8413 26", True),
    ("GB29 RBOS 6016 1331 9268 19", False), -- INVALID NUM
    ("GB86 RBOS 6016 1331 9268 19", True), -- Now valid NUM
    ("BA39 1290 0794 0102 8494", True),
    ("CZ65 0800 0000 1920 0014 5399", True)
                                         ]
            ]

invalidIBANTests :: [Test]
invalidIBANTests = [ Test "Invalid Tests" testIban [
    ("AL47 2121 1009 0000 0202 3569 8741", False),
    ("AD12 0001 2030 2003 6910 0100", False),
    ("FR14 2004 1010 0505 2001 3M02 606", False),
    ("HU42 1177 3016 1131 1018 0000 0000", False),
    ("JO94 CBJO 0010 0000 0000 0131 0004 02", False),
    ("MT84 MALT 0110 0051 2345 MTLC AST0 01S", False),
    ("QA58 DOHC 0000 1234 5678 90AB CDEF G", False),
    ("NL39 RAB 0300 0652 64", False),
    ("NO94 8601 1117 947", False),
    ("RO49 AAAAA 1B31 0075 9384 0000", False),
    ("MU17 BO 0101 1010 3030 0200 000M UR", False),
    ("KW81 BBKU 0000 0000 0000 1234 5601 01", False),
    ("MK072 6012 0000 0589 84", False),
    ("TR33 0007 1005 1978 6457 8413 26", False),
    ("GB29 RBOS 6036 1331 9268 19", False),
    ("BA39 1290 0794 0102 8495", False),
    ("CZ65 0800 0000 1920 0014 5390", False)
                                         ]
            ]
-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ validIBANTests
                  , invalidIBANTests
                  ]
                  
{- Properties -}

prop_samelength :: String -> String -> Bool
prop_samelength a b= length a == length b

prop_iban :: String -> Bool
prop_iban ib =  rem banknr 97 == 1 
    where banknr = (read . convertAlphaNumeric . moveChar 4  . trimSpaces) ib
    
moveChar :: Int -> String -> String
moveChar _ [] = []
moveChar n (x:xs)= if n > 0 then moveChar (n - 1) (xs ++ [x]) else x : xs
{- (Mostly) automated Testing -}

randomFlip :: Int -> IO Int
randomFlip x = do 
   b <- getRandomInt 1
   if b == 0 then return x else return (-x)

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))
                    
genIntList :: IO [Int]
genIntList = do 
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n
  
getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)                

countryCodes :: [String]
countryCodes = ["NL","GB","FR","FO", "GR", "GL"]

pick :: [a] -> IO a
pick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

genCountryCode :: IO String
genCountryCode = pick countryCodes

genAccountNum :: IO Integer
genAccountNum = getStdRandom (randomR (10000,10000000))
                  
genBankAccount :: IO String
genBankAccount = do
            x <- genAccountNum
            let acc = show x
            return acc
            
genIBAN ::  IO String
genIBAN = do 
              cc <- genCountryCode
              bb <- genBankAccount
              let accnum = cc ++ genCheckDigit (cc ++ "00" ++ bb) ++ bb
              return accnum
              
-- ^ Generates a valid IBAN number 
       
genInvalidIBAN ::  IO String
genInvalidIBAN = do 
                cc <- genIBAN
                let accnum = (take 4 cc) ++ (show . (+1) . read) (drop 4 cc)
                return accnum   
-- ^ Gen invalid IBAN       
                         
                            
genCheckDigit :: String -> String
genCheckDigit = calculateCheckDigits

genIBANList :: IO [String]
genIBANList = getIBANL 1 100

getIBANL :: Int -> Int -> IO [String]
getIBANL _ 0 = return []
getIBANL k n = do 
   x <-  genIBAN
   xs <- getIBANL k (n-1)
   return (x:xs) 

test_valid :: Int -> Int -> (String -> Bool) -> (String -> Bool) -> IO ()
test_valid k n f r = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- genIBAN
                  if r xs == f xs then
                    do print (show (k+1) ++ ". pass on: " ++ show xs ++ " IBAN is valid: " ++ show (r xs))
                       test_valid (k+1) n f r
                  else error ("failed test on: " ++ show xs)    
          
test_invalid :: Int -> Int -> (String -> Bool) -> IO ()
test_invalid k n f = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- genInvalidIBAN
                  if f xs == False then
                    do print (show (k+1) ++ ". pass on: " ++ show xs ++ " IBAN is valid: " ++ show (f xs))
                       test_invalid (k+1) n f
                  else error ("failed test on: " ++ show xs)   
                  
                  
                  