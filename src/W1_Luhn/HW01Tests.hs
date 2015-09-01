-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module W1_Luhn.HW01Tests where

import W1_Luhn.HW01
import W1_Luhn.Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (n, xs) = toRevDigits n == xs

ex2Tests :: [Test]
ex2Tests = [Test "toRevDigits test" testToRevDigits 
    [(40235223, [3,2,2,5,3,2,0,4]), (203, [3,0,2]), (5594589764218858, [8,5,8,8,1,2,4,6,7,9,8,5,4,9,5,5])]]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (xs, ys) = doubleEveryOther xs == ys

ex3Tests :: [Test]
ex3Tests = [Test "doubleEveryOther test" testDoubleEveryOther
    [([1,2,3,4,5,6,7,8,9],[1,4,3,8,5,12,7,16,9]),([3,7,8,9,4,3,3,3,1,1],[3,14,8,18,4,6,3,6,1,2]),([5,6,2,4,3,7,8],[5,12,2,8,3,14,8])]
    ]

-- Exercise 4 -----------------------------------------

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (xs, n) = sumDigits xs == n

ex4Tests :: [Test]
ex4Tests = [Test "testSumDigits test" testSumDigits
    [([1,2,3,4,5,6,7,8,9], 45),([3,7,8,9,4,3,3,3,1,1], 42),([5,6,2,4,3,7,8], 35)]
    ]

-- Exercise 5 -----------------------------------------

testLuhn :: (Integer, Bool) -> Bool
testLuhn (n, x) = luhn n == x


ex5Tests :: [Test]
ex5Tests = [Test "testLuhn test" testLuhn
    [(79927398713, True),(1234567898765432, False),(79927398712,False), (5594589764218858, True)]
    ]


-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  ]