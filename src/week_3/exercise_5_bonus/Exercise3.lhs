> module Exercise3 where

The lecture notes of this week discuss the conversion of Boolean formulas (formulas of propositional logic) into CNF form. 
The lecture notes also give a definition of a Haskell datatype for formulas of propositional logic, using lists for conjunctions and disjunctions. 
Your task is to write a Haskell program for converting formulas into CNF.


Deliverables: conversion program with documentation, indication of time spent.

> import Lecture3
> import Test.QuickCheck
> import Data.List

After making the formula arrowfree and pushing the negation inward (resulting from the nnf function in lecture 3) 
It's necessary to use distribution to modify the formula so it will only contain conjunctions of disjunctions (a OR b ) AND (c OR d) for example

> distribution :: Form -> Form
> distribution (Prop x) = Prop x 
> distribution (Neg (Prop x)) = Neg (Prop x)
> distribution (Cnj fs) = Cnj (map distribution fs)
> distribution (Dsj fs) = Dsj (map distribution fs)
> distribution (Dsj [f1, Cnj [f2, f3]]) = Cnj [ distribution (Dsj[distribution f1, distribution f2]), (Dsj[distribution f2, distribution f3])]
> distribution (Dsj [Cnj [f1, f2], f3]) = Cnj [ distribution (Dsj[distribution f1, distribution f3]), (Dsj[distribution f2, distribution f3])]

> cnf :: Form -> Form 
> cnf = (distribution . nnf . arrowfree) 

> cnftestform :: Form 
> cnftestform = Equiv (Impl (Prop 1)  (Prop 2)) (Impl (Neg (Prop 2)) (Neg (Prop 1))) 

Tried some example functions from the workshop before implementing automatic testing

> cnfWorkshop6_1 = Neg(Neg(Neg p))
> cnfWorkshop6_2 = Neg(Dsj[p, Neg(q)])
> cnfWorkshop6_3 = Neg(Cnj[Neg(q),Neg(p)])
> cnfWorkshop6_4 = Equiv (Impl (Prop 1)  (Prop 2)) (Impl (Neg (Prop 2)) (Neg (Prop 1)))


logical equivalence from exercise one

> equiv :: Form -> Form -> Bool
> equiv f1 f2 = all (\x -> (evl x form)) (allVals form)
>    where form = Equiv f1 f2

> testCNFConversion :: Form -> Bool
> testCNFConversion f = equiv f f'
>   where f' = cnf f


Time spent on exercise 3: 1 hour


Exercise 4: 

 Automatic formula generator from exercise 2

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

 
Auto test CNF conversion 1000 times with moderate complexity (maxSize 30) 

> autoTestProperty :: (Form -> Bool) -> IO()
> autoTestProperty p = quickCheckWith stdArgs {maxSize = 30, maxSuccess = 1000 } p

Properties

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



Time spent: 30 minutes (already created generator in exercise 2) 

