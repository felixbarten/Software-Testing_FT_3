> module Exercise1 where


> import Lecture3

> contradiction :: Form -> Bool
> contradiction f = not(satisfiable f)

> tautology :: Form -> Bool
> tautology f = all (\ v -> evl v f) (allVals f)

> entails :: Form -> Form -> Bool
> entails x y = tautology (Impl x y) 

> equiv :: Form -> Form -> Bool
> equiv x y =  tautology (Equiv x y)
