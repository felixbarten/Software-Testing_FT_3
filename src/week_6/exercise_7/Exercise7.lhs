> module Exercise7 where

> import Lecture6
> import Control.Monad
> import Control.Monad.Loops
> import System.Random

Report
======

Generates a random pair of primes for a particular bitsize can be used to make huge prime numbers

> genRandomPrime :: Integer -> IO Integer
> genRandomPrime b = do
>     x <- randomPrime 
>     r <- primeMR 100 x
>     if r then return x else genRandomPrime b
>     where randomPrime  = randomRIO (2^(b-1), 2^b-1) 

> generateSameBitPairs :: Integer -> IO (Integer, Integer)
> generateSameBitPairs b = do
>     c1 <- genRandomPrime b
>     c2 <- genRandomPrime b
>     return (c1,c2) 


