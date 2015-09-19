> module Exercise2 where

Testing module for Lecture3 Parse merthod

> import Lecture3
> import Test.QuickCheck


> instance Arbitrary Form where
>   arbitrary   = sized form



> form :: Int -> Gen Form 
> form 0 = do
>       x <- choose(1,10) :: Gen Int 
>       return (Prop x)
> form n | n > 0= do
>       x <- choose (1,10) :: Gen Int
>       y <- choose (1,10) :: Gen Int
>       p <- choose (1,10) :: Gen Int
>       choice <- choose (0,4) :: Gen Int
>       case (choice) of
>           0 -> return $ Prop x
>           1 -> do 
>               f2 <- listOf subform
>               return $ Cnj $ f2
>           2 -> do 
>               f2 <- listOf subform
>               return $ Dsj $ f2
>           3 -> do
>               f1 <- subform
>               f2 <- subform
>               return $ Impl (f1) (f2) 
>           4 -> do
>               f1 <- subform
>               f2 <- subform
>               return $ Equiv (f1) (f2)
>       where subform = form (n `div` 2) 

Example test

> deftest = verboseCheckResult (\x -> satisfiable x)


> parse' :: Form -> Bool
> parse' x = (parse $ show x) == (parse . show . head) (parse $ show x)

Custom args for our tests.
If the maxSize exceeds 40 the random data structures might not finish calculation and may result in crashing computers ;)

> customArgs :: Args
> customArgs = stdArgs { maxSize = 35, maxSuccess = 100, chatty = True } 

Test function for the parse functionality can be found below

> parseTest = quickCheckWith customArgs parse'

Test Report

Output for tests:

*Exercise2> parseTest
Loading package array-0.5.0.0 ... linking ... done.
Loading package deepseq-1.3.0.2 ... linking ... done.
Loading package bytestring-0.10.4.0 ... linking ... done.
Loading package Win32-2.3.0.2 ... linking ... done.
Loading package old-locale-1.0.0.6 ... linking ... done.
Loading package time-1.4.2 ... linking ... done.
Loading package random-1.0.1.1 ... linking ... done.
Loading package containers-0.5.5.1 ... linking ... done.
Loading package pretty-1.1.1.1 ... linking ... done.
Loading package template-haskell ... linking ... done.
Loading package QuickCheck-2.6 ... linking ... done.
+++ OK, passed 100 tests.

Time spent on exercise two: 5,5 hours