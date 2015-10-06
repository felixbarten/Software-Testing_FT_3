> module Exercise4 where



> import Lecture6
> import Exercise3 
> import Control.Monad


> testFermat k = filterM (\c -> tstFer k c) composites

 
> tstFer k n = do
>                result <- prime_tests_F k n
>                when (result) $ print ("True for composite " ++ show n ++ " and K: " ++ show k) 
>                return result

When increasing the K number the accuracy of the primality check 


*Exercise4> testFermat 90
"True for composite 2508013 and K: 90"
^CInterrupted.
*Exercise4> Lecture6.isPrime 2508013
False
*Exercise4> 
