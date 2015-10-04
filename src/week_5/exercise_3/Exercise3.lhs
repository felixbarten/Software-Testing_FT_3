> module Exercise3 where

> import Lecture5

Report
======

The easiest way to do this is by following the exact steps in the exercise?...

*Timespent: 30 mins*
======



This function attempts to remove all hints and see if the result still has 1 unique solution

> testMinimal :: Node -> Bool
> testMinimal n = and $ map (\(x,y) -> (not.uniqueSol) $ eraseN  n (x,y)) [(r,c)| (r,c) <- filledPositions s]
>                       where s = fst n

> test :: IO Bool
> test = do 
>          [r] <- rsolveNs [emptyN]
>          s  <- genProblem r
>          return $ testMinimal s








