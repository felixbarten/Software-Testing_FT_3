> module Exercise1 where

> import Data.List
> import System.Random

> import Lecture3

> contradiction :: Form -> Bool
> contradiction f = all (\ v -> not(evl v f)) (allVals f)

> tautology :: Form -> Bool
> tautology f = all (\ v -> evl v f) (allVals f)

> entails :: Form -> Form -> Bool
> entails f1 f2 = all(\v -> not(evl v f1) || evl v f2) (allVals (Impl f1 f2) )

> equiv :: Form -> Form -> Bool
> equiv f1 f2 = all(\v -> evl v f1 == evl v f2) ( allVals (Equiv f1 f2) ) 