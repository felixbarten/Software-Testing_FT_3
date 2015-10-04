> module Exercise3 where

> import Lecture5


Test if the soduku solutions are minimal


First get a solved sudoku 

Then check each non empty field (hint) if it can be erased and if the outcome is more than one solution if all fields pass then it's a minimal sudoku

> type Position = (Row,Column)

Same code as main, generate a problem, show it and then try to check if it's minimal.

> testMinimal :: IO Bool
> testMinimal = do 
>               [r] <- rsolveNs [emptyN]
>               showNode r
>               s  <- genProblem r
>               showNode s
>               return $ checkMinimal s



Check whether solutions are unique if any of the positions are erased. 

> checkMinimal :: Node -> Bool
> checkMinimal (sud, _) = foldr (\x acc -> checkMinimal' x sud && acc) True [(r,c)| (r,c) <- filledPositions sud]

Because we don't want to modify the sudoku parameter pass it to a different function.

> checkMinimal' :: Position -> Sudoku -> Bool
> checkMinimal' p s = uniqueSol ((eraseS s p), [])

Manual testing for minimal sudoku's

> testMinimalSud :: Int -> Int -> IO ()
> testMinimalSud k n = if k == n then print (show n ++ " tests passed")
>                 else do
>                   tst <- testMinimal
>                   if tst then
>                     do print ("Passed Test!" ++ show tst)
>                        testMinimalSud (k+1) n 
>                   else error ("failed test!"++ show tst)

Test report 


*Exercise3> testMinimalSud 1 50
+-------+-------+-------+
| 1 2 6 | 3 7 5 | 4 8 9 |
| 5 7 8 | 2 9 4 | 3 6 1 |
| 4 9 3 | 6 1 8 | 5 7 2 |
+-------+-------+-------+
| 9 1 4 | 5 6 7 | 8 2 3 |
| 2 6 5 | 9 8 3 | 1 4 7 |
| 8 3 7 | 4 2 1 | 9 5 6 |
+-------+-------+-------+
| 6 5 1 | 7 4 9 | 2 3 8 |
| 7 4 9 | 8 3 2 | 6 1 5 |
| 3 8 2 | 1 5 6 | 7 9 4 |
+-------+-------+-------+
+-------+-------+-------+
|       |   7   |     9 |
|     8 | 2   4 |       |
| 4     | 6   8 |   7   |
+-------+-------+-------+
|     4 |       |       |
| 2 6   |     3 | 1     |
|       |     1 | 9   6 |
+-------+-------+-------+
| 6     | 7 4   | 2     |
| 7 4   |       |       |
| 3     |       |       |
+-------+-------+-------+
"Passed Test!True"
"50 tests passed"
*Exercise3>

Time spent: About one hour