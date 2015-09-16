> module Exercise3 where

The lecture notes of this week discuss the conversion of Boolean formulas (formulas of propositional logic) into CNF form. The lecture notes also give a definition of a Haskell datatype for formulas of propositional logic, using lists for conjunctions and disjunctions. Your task is to write a Haskell program for converting formulas into CNF.

Deliverables: conversion program with documentation, indication of time spent.

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

 
Time spent: tree fiddy hours