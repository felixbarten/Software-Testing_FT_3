> module Exercise4 where

> import Lecture6
> import Control.Monad


Report
=====

The smallest possible number that it can fail for is 4. Incrementing the value of k
results in improving the accuraccy of the function

==============



> findCE :: Int -> IO Integer
> findCE n = do
>            x <- prime_test_F (composites !! n)
>            print x  
>            print (composites !! n)
>            if x then return (composites !! n) else findCE (n+1) 

> testSmallestExample :: Int -> IO ()
> testSmallestExample n = do
>                         x <- prime_tests_F n 4
>                         print n 
>                         if x then print n else testSmallestExample (n+1)


> testS :: Int -> IO ()
> testS n = do
>                         x <- prime_tests_F n 4
>                         print n
>                         print x 
