> module Exercise62 where

> import Lecture6
> import Control.Monad
> import Control.Applicative

Report 
======

It is possible to find the first 21 Mersenne primes within 10 minutes using the
mersenneStuff function21 Mersenne primes within 10 minutes using the
mersenneStuff function.
The MR test seems to be sufficiently accurate in finding the numbers if k is
provided a value that is greated than 4 during the test.



> mersenneStuff :: Int -> IO()
> mersenneStuff n  = do 
>                 x <- findMersennePrimes n
>                 print $ show $ x
>                 print ("length: " ++ (show $ (length x)))

> findMersennePrimes :: Int ->  IO [Int]
> findMersennePrimes n = do    
>                   mps <- take n <$> filterM (mersenneCheck) ps  
>                   return mps
>                   where ps = map (fromIntegral) (takeWhile (< 9942) primes)


> mersenneCheck :: Int -> IO Bool
> mersenneCheck n = do
>             x <- primeMR 100 (2^n - 1) 
>             return x



