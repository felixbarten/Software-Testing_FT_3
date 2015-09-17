> module Exercise1 where

> import Lecture3
> import Data.List
> import Test.QuickCheck

Report: Exercise 1
==========

The method of checking the entails and equiv functions is by first rewriting them to their arrowfree form and then ensuring that applying the tautology function to that 
new form has the same result as using entails and equiv on the original parameters.

> testEntails :: Bool
> testEntails = tautology (arrowfree (Impl form1 form2)) == entails form1 form2 

> testEquiv :: Bool
> testEquiv = tautology (arrowfree (Equiv form1 form2)) == entails form1 form2 

*Time Spent: 2 hour*

> tautology :: Form -> Bool
> tautology x = all (\p -> evl p x) $ allVals x

> contradiction :: Form -> Bool
> contradiction = (not . satisfiable)

> entails :: Form -> Form -> Bool
> entails x y = tautology (Impl x y) 

> equiv :: Form -> Form -> Bool
> equiv x y =  tautology (Equiv x y) 
