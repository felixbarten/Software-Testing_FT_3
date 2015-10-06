> module Exercise7 where


> import Lecture6
> import Control.Monad

Expects a list of primes

> mersenne k (p:ps) = do 
>                      res <- primeMR k ((2^p)-1) 
>                      when (res) $  print ("Prime: " ++ show p ++ " is a mersenne prime")
>                      mersenne k ps
>                            

Output:

*Exercise7> mersenne 5 Lecture6.primes
"Prime: 2 is a mersenne prime"
"Prime: 3 is a mersenne prime"
"Prime: 5 is a mersenne prime"
"Prime: 7 is a mersenne prime"
"Prime: 13 is a mersenne prime"
"Prime: 17 is a mersenne prime"
"Prime: 19 is a mersenne prime"
"Prime: 31 is a mersenne prime"
"Prime: 61 is a mersenne prime"
"Prime: 89 is a mersenne prime"
"Prime: 107 is a mersenne prime"
"Prime: 127 is a mersenne prime"
"Prime: 521 is a mersenne prime"
"Prime: 607 is a mersenne prime"
"Prime: 1279 is a mersenne prime"
"Prime: 2203 is a mersenne prime"
"Prime: 2281 is a mersenne prime"
"Prime: 3217 is a mersenne prime"
"Prime: 4253 is a mersenne prime"
"Prime: 4423 is a mersenne prime"
^CInterrupted.
*Exercise7> 


