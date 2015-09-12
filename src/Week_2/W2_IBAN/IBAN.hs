module Week_2.W2_IBAN.IBAN where
import Data.Char
import System.Random
import Data.List

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
