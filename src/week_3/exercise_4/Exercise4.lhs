> module Exercise4 where

> import Exercise3
> import Lecture3
> import Test.QuickCheck
> import Control.Monad
> import Data.List


Test Report 
==========

The toCnf function was tested using the same techniques used in exercise 2.  An arbitrary form instance is declared and
that is used to test the function automagically using quickCheck. The tests are conducted using the property that a particular
prop logic formula is equivalent to its cnf form.

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

logical equivalence from exercise one

> equiv :: Form -> Form -> Bool
> equiv f1 f2 = all (\x -> (evl x form)) (allVals form)
>    where form = Equiv f1 f2


Auto test CNF conversion 1000 times with moderate complexity (maxSize 30) 

> autoTestProperty :: (Form -> Bool) -> IO()
> autoTestProperty p = quickCheckWith stdArgs {maxSize = 10, maxSuccess = 1000 } p

Properties


Check if a formula is a tautology

> prop_tautology :: Form -> Bool
> prop_tautology f = all (\x -> evl x f) (allVals f)


Check if form contains no Impl or Equiv.

> prop_arrowfree :: Form -> Bool
> prop_arrowfree f = not (isInfixOf "<==" f' || isInfixOf "<=>" f' || isInfixOf "==>" f')
>   where f' = (show . arrowfree) f

Check if formula is logically equivalent to cnf version of the same formula

> prop_equivalent :: Form -> Bool
> prop_equivalent f = equiv f f'
>   where f' = cnf f


Test report: 

Checked two properties for this exercise

*Exercise3> autoTestProperty prop_arrowfree
+++ OK, passed 1000 tests.
*Exercise3>

To make sure the formulae were arrowfree the property was created to look for arrows in the formula.

After that it was important to check whether the formula's that were converted to the CNF form were still logically equivalent to their non CNF counterpart

*Exercise3> autoTestProperty prop_equivalent
+++ OK, passed 1000 tests.
*Exercise3>



Time spent: 45 minutes (already created generator in exercise 2) 
