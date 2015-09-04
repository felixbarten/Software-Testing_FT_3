CIS 194, Spring 2015
===================

Test cases for HW 01
===================

> module HW01Tests where
> 
> import HW01
> import Testing

*Exercise 1* 

> testLastDigit :: (Integer, Integer) -> Bool
> testLastDigit (n, d) = lastDigit n == d
> 
> testDropLastDigit :: (Integer, Integer) -> Bool
> testDropLastDigit (n, d) = dropLastDigit n == d
> 
> ex1Tests :: [Test]
> ex1Tests = [ Test "lastDigit test" testLastDigit
>              [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
>            , Test "dropLastDigit test" testDropLastDigit
>              [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
>            ]

*Exercise 2* 

> testToRevDigits :: ([Integer], Integer) -> Bool
> testToRevDigits (ns, n) = toRevDigits n == ns
> 
> ex2Tests :: [Test]
> ex2Tests = [ Test "toRevDigits test" testToRevDigits [([2,3,4], 432),([1,3,2], 231)]]

*Exercise 3* 

> testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
> testDoubleEveryOther (ns, n) = doubleEveryOther n == ns
> 
> ex3Tests :: [Test]
> ex3Tests = [ Test "toDoubleEveryOther test" testDoubleEveryOther [ ([2,6,4], [2,3,4]), ([1,6,2,16], [1,3,2,8]), ([1], [1])]]

*Exercise 4*

> testSumDigits :: (Integer, [Integer]) -> Bool
> testSumDigits (ns, n) = sumDigits n == ns
> 
> ex4Tests :: [Test]
> ex4Tests = [ Test "SumDigits test" testSumDigits [ (9, [2,3,4]), (14, [1,3,2,8]), (1, [1]),(0, [])]]

*Exercise 5* 

> testLuhn :: (Bool, Integer) -> Bool
> testLuhn (ns, n) = luhn n == ns
> 
> ex5Tests :: [Test]
> ex5Tests = [ Test "luhn test" testLuhn [( True, 5594589764218858), (False,24324242)]]

*Exercise 6* 

> ex6Tests :: [Test]
> ex6Tests = []

*All Tests*

> allTests :: [Test]
> allTests = concat [ ex1Tests
>                   , ex2Tests
>                   , ex3Tests
>                   , ex4Tests
>                   , ex5Tests
>                   , ex6Tests
>                   ]
