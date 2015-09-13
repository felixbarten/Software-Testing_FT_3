module IBAN where

-- Test Report IBAN
--  
-- The automatic tests for iban were created by creating a number of functions that made generating random valid IBAN numbers possible.
-- Furthermore to get invalid examples the previous functions were extended by incrementing the account number after the calculations for the bank account were completed.
-- This method ensures the iban is invalid when testing for invalidity.
--  
-- Console output:
--  
-- *IBAN> testValidIBAN 100
-- "*** pass on: AT547431007"
-- "*** pass on: AZ1013378060"
-- "*** pass on: BG627199088"
-- "*** pass on: GR866627280"
-- "*** pass on: AT661512336"
-- "*** pass on: AZ662733824"
-- "*** pass on: BH442929418"
--
--
-- *IBAN> testFalseIBAN  100
-- "*** pass on: BG271448783"
-- "*** pass on: BA2512533817"
-- "*** pass on: BG3511799993"
-- "*** pass on: BE0810817395"
-- "*** pass on: AT6910827968"
-- "*** pass on: AZ316331990"

import Data.Char
import System.Random
import Data.List
import Testing

countries = ["AL","AD", "AT","AZ","BH","BE","BA","BG","GR"]

strToInteger :: String -> Integer 
strToInteger s = read s :: Integer

genAccountNum :: IO Integer
genAccountNum = getStdRandom (randomR (80000,15000000))
                  
genBankAccount :: IO String
genBankAccount = do
            x <- genAccountNum
            let acc = show x
            return acc

thirdAndFourthDigit::  String -> String
thirdAndFourthDigit ib = if (98 - rem iban 97 < 10) then "0" ++ show (98 - rem iban 97) else show (98 - rem iban 97)
    where iban = (strToInteger . convertLettersToNumerics. moveCharactersToRight .  convertToBasicFormat) ib
            

-- below method creates a valid IBAN 

genTestIban ::  IO String
genTestIban = do 
              country <- pickCountry
              account <- genBankAccount
              let final_iban = country ++ thirdAndFourthDigit  (country ++ "00" ++ account) ++ account 
              return final_iban

-- below method creates an invalid IBAN by incrementing last digit of a valid iban by 1

genFakeIban ::  IO String
genFakeIban = do
              country <- pickCountry
              account <- genBankAccount
              let final_iban =  country ++ thirdAndFourthDigit  (country ++ "00" ++ account) ++ increaseOne(account)
              return final_iban

increaseOne:: String -> String
increaseOne x = (show . (+1) . read) x
 

pickCountry:: IO String
pickCountry = randomRIO (0, length(countries) - 1) >>= return . (countries !!)

-- *** below method tests if a given string is iban

iban :: String ->Bool
iban [] = False
iban x = applyMod97 (convertLettersToNumerics (moveCharactersToRight (convertToBasicFormat x)))


convertToBasicFormat :: String -> String
convertToBasicFormat [] = []
convertToBasicFormat (x:xs) =
        if isAlphaNum x
        then x:convertToBasicFormat xs
        else convertToBasicFormat xs


moveCharactersToRight :: String -> String
-- below needed?
moveCharactersToRight [] = []
moveCharactersToRight s = drop 4 s ++ take 4 s

convertLettersToNumerics:: String -> String
convertLettersToNumerics [] = []
convertLettersToNumerics (x:xs) =
        if isAlpha x
        then convertLetter x ++ convertLettersToNumerics xs
        else x:convertLettersToNumerics xs

convertLetter:: Char -> String
convertLetter x
                | x == 'A' = "10"
                | x == 'B' = "11"
                | x == 'C' = "12"
                | x == 'D' = "13"
                | x == 'E' = "14"
                | x == 'F' = "15"
                | x == 'G' = "16"
                | x == 'H' = "17"
                | x == 'I' = "18"
                | x == 'J' = "19"
                | x == 'K' = "20"
                | x == 'L' = "21"
                | x == 'M' = "22"
                | x == 'N' = "23"
                | x == 'O' = "24"
                | x == 'P' = "25"
                | x == 'Q' = "26"
                | x == 'R' = "27"
                | x == 'S' = "28"
                | x == 'T' = "29"
                | x == 'U' = "30"
                | x == 'V' = "31"
                | x == 'W' = "32"
                | x == 'X' = "33"
                | x == 'Y' = "34"
                | x == 'Z' = "35"

applyMod97:: String -> Bool
applyMod97 [] = False
applyMod97 x  = finalInt `mod` 97 == 1
                where finalInt = read x:: Integer

-- below methods for testing

-- sample:: testValidIBAN 100
--sample:: testFalseIBAN 100

testValidIBAN :: Int -> IO ()
testValidIBAN n = resultsIBAN 1 n

testFalseIBAN :: Int -> IO ()
testFalseIBAN n = false_resultsIBAN 1 n

resultsIBAN :: Int -> Int -> IO()
resultsIBAN k n = if k == n then print (show n ++ " tests passed")
                else do
                  test <- genTestIban
                  if (iban(test)) then
                    do print ("*** pass on: " ++ test )
                       resultsIBAN (k+1) n
                  else error ("*** failed test on: " ++ test)

false_resultsIBAN :: Int -> Int -> IO()
false_resultsIBAN k n = if k == n then print (show n ++ " tests passed")
                else do
                  test <- genFakeIban
                  if not(iban(test)) then
                    do print ("*** pass on: " ++ test )
                       resultsIBAN (k+1) n
                  else error ("*** failed test on: " ++ test)
                  
                  
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
allTests = validIBANTests ++ invalidIBANTests                 

