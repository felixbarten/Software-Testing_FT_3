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
