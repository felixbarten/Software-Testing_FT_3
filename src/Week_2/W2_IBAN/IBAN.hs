module Week_2.W2_IBAN.IBAN where
import Data.Char


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
