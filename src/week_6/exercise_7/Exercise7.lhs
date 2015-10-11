> module Exercise7 where

> import Lecture6
> import Control.Monad


> findMersenne :: Int -> [Integer] -> IO()
> findMersenne n (p:ps) = do 
>                      res <- primeMR n ((2^p)-1) 
>                      when (res) $  print (show p ++ " is mersenne prime")
>                      findMersenne n ps
