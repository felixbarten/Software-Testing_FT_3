module Week_2.W2_Euler_bonus.Euler where

-- euler 1

calcSumOfMultiOfThreeOrFive :: Integer  
calcSumOfMultiOfThreeOrFive = sum . takeWhile (<1000) . filter divi $ [1..]

divi :: Integer -> Bool
divi x = x `mod` 5 == 0 || x `mod` 3 == 0

-- euler 6

sumOfSquares :: Integer  -> Integer
sumOfSquares x = sum (map (^2) [1..x])

squareOfSums:: Integer -> Integer
squareOfSums x = (sum [1..x]) ^2

calculateDifference :: Integer -> Integer
calculateDifference x =  squareOfSums x - sumOfSquares x

-- result: 25164150
