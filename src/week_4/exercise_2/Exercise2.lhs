> module Exercise2 where

> import SetOrd
> import System.Random

let list = genIntList

*** random data generator ***

> generateSet :: IO (Set Int)
> generateSet =  do 
>              seed <- getStdGen
>              length <-getStdRandom(randomR(0,20))
>	       let list = take length (randoms seed:: [Int]) 
>              return (Set list)


so far... 1.5hr...:@:@
