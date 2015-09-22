> {-# LANGUAGE FlexibleContexts #-}


> module Exercise8 where

Testing of symmetric and transitive closure


> import Closures
> import Test.QuickCheck
> import System.Random

test if precedence of creating the symmetric and transitive closures is important

> testClosures :: Ord Int => Rel Int -> Bool
> testClosures r = (trClos . symClos) r == (symClos . trClos) r

Convenient test method

> testClosurePrecedence :: IO Result
> testClosurePrecedence = quickCheckWithResult stdArgs testClosures

The order of these operations is definitely important because the test above will fail

Output:
*Exercise8> testClosurePrecedence 
*** Failed! Falsifiable (after 2 tests and 1 shrink):     
[(0,1)]
Failure {numTests = 2, numShrinks = 1, numShrinkTries = 1, numShrinkFinal = 2, usedSeed = TFGenR 00000000801C799D000000000000C350000000000000DFC700008CC586EB1800 0 6 3 0, USEDSIZE = 1, REASON = "FALSIFIABLE", THEEXCEPTION = NOTHING, LABELS = [], OUTPUT = "*** FAILED! FALSIFIABLE (AFTER 2 TESTS AND 1 SHRINK): \N[(0,1)]\N"}
*Exercise8> 

If we take the smallest set that didn't conform to the test we get [(0,1)]

if the transitive closure from this relation is taken first and then the symmetric one it will result in this [(0,1),(1,0)]
If these operations are done the other way around it's [(0,1),(0,0),(1,0),(1,1)] 

So the precedence of these closures definitely matters. 
