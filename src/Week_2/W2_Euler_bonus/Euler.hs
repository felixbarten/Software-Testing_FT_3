module Week_2.W2_Euler_bonus.Euler where

-- euler 1

calcSumOfMultiOfThreeOrFive :: Integer  
calcSumOfMultiOfThreeOrFive = sum . takeWhile (<1000) . filter divi $ [1..]

divi :: Integer -> Bool
divi x = x `mod` 5 == 0 || x `mod` 3 == 0
