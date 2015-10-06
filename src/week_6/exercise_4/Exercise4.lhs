> module Exercise4 where

> import Lecture6
> import Control.Monad


> composites :: [Integer]
> composites = filter composite [2..]

> composite :: Integer -> Bool
> composite n = any (\ x -> rem n x == 0) xs
>   where xs = takeWhile (\ y -> y^2 <= n) [2..]

> isComposite :: Integer -> Bool
> isComposite x = x `elem` composites


checkFermat :: Int -> Integer -> Bool
checkFermat k n = prime_tests_F k n /= isComposite n

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


