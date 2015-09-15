> module Exercise1 where

Module

> import Lecture3

Satisfiable

> satisfiable :: Form -> Bool
> satisfiable f = any (\ v -> evl v f) (allVals f)


contradiction

> contradiction :: Form -> Bool
> contradiction f = all (\x -> not(evl x f)) (allVals f)

Tautology

> tautology :: Form -> Bool
> tautology f = all (\x -> evl x f) (allVals f)

logical entailment 

> entails :: Form -> Form -> Bool
> entails f1 f2 = all (\x -> evl x $ form) (allVals form)
>   where form = Impl f1 f2

    
logical equivalence

> equiv :: Form -> Form -> Bool
> equiv f1 f2 = all (\x -> (evl x form)) (allVals form)
>    where form = Equiv f1 f2

These methods were checked with a few of the predefined formulas included in the Lecture 3. 
I have created an additional form to check the contradiction and tautologies. See code sample below

> form4 :: Form
> form4 = Cnj [Prop 1, Neg (Prop 1)] 

Equiv tester

> form5 :: Form
> form5 = Equiv (Prop 1) (Prop 1)


This gives:

 *Exercise1> tautology form5
 True
 *Exercise1> contradiction form4
 True
 *Exercise1> equiv form4 form5 
 False -- Should return false because all T is not equivalent to all F
 *Exercise1> entails form4 form5
 True
 *Exercise1> equiv form1 form5
 True
 
 
 Time spent: 1 hour
 

