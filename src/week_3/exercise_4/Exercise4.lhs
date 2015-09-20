> module Exercise4 where

> import Exercise3
> import Lecture3
> import Test.QuickCheck
> import Control.Monad

Test Report 
==========

The toCnf function was tested using the same techniques used in exercise 2.  An arbitrary form instance is declared and
that is used to test the function automagically using quickCheck. The tests are conducted using the property that a particular
prop logic formula is equivalent to its cnf form.

Time Spent: 45 mins


This declaration was taken form exercise 3

> instance Arbitrary Form where
>    arbitrary = form
> form = sized form'
> form' 0 = liftM Prop (choose(1,10) :: Gen Int)
> form' n | n > 0 = oneof [
>                     liftM Neg subform,
>                     liftM Cnj (listOf subform),
>                     liftM Dsj (listOf subform),
>                     liftM2 Impl subform subform,
>                     liftM2 Equiv subform subform
>                     ]
>               where subform = form' (n `div` 2)


> tautology :: Form -> Bool
> tautology x = all (\p -> evl p x) $ allVals x

> equiv :: Form -> Form -> Bool
> equiv x y =  tautology (Equiv x y) 

> prop_cnf_equivalence :: Form -> Bool
> prop_cnf_equivalence f = let r = cnf f
>                          in equiv f r

> test = quickCheckWith stdArgs {maxSize = 10}  
> testToCnf = test prop_cnf_equivalence
