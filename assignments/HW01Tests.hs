-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

testToRevDigist :: (Integer, [Integer]) -> Bool
testToRevDigist (n, d) = toRevDigits n == d

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (n, d) = doubleEveryOther n == d

testMakeProducts :: ([Integer], [Integer]) -> Bool
testMakeProducts (n, d) = makeProducts n == d

testLuhn :: (Integer, Bool) -> Bool
testLuhn(n, d) = luhn n == d


testSumDigits :: ([Integer] , Integer) -> Bool
testSumDigits (n, d) = sumDigits n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = [Test "toRevDigits test" testToRevDigist
				[(123, [3,2,1]),(321,[1,2,3])]
				]

-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests = [Test "doubleEveryOther test" testDoubleEveryOther
				[([5,3,1,5],[5,6,1,10]), ([7,7,6,2],[7,14,6,4])]
				, Test "makeProducts test" testMakeProducts
				[([5,14,2,18],[5,5,2,9]), ([7,11,6,12],[7,2,6,3])]
				]

-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests = [Test "sumDigits test" testSumDigits
				[([1,2,3,4], 10), ([2,3,1,5], 11)]
				]

-- Exercise 5 -----------------------------------------

ex5Tests :: [Test]
ex5Tests = [Test "luhn test" testLuhn
				[(5594589764218858, True), (1234567898765432, False), (4929825602520082, True)]
				]

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests = []

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
