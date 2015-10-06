> module Exercise3 where
 
*** Time spent 5mins . . . ***

> composites :: [Integer]
> composites = filter composite [2..]

> composite :: Integer -> Bool
> composite n = any (\ x -> rem n x == 0) xs
>   where xs = takeWhile (\ y -> y^2 <= n) [2..]
