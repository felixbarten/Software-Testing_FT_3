  module Exercise3 where

  import Lecture5



{- Main function has been overwritten

-- Time spent : 3hours . . . --


Output is shown below:

+-------+-------+-------+
| 8 9 5 | 7 6 3 | 4 1 2 |
| 4 7 3 | 2 1 8 | 9 6 5 |
| 6 2 1 | 4 9 5 | 8 7 3 |
+-------+-------+-------+
| 1 6 4 | 3 5 9 | 2 8 7 |
| 9 8 7 | 6 2 4 | 5 3 1 |
| 3 5 2 | 8 7 1 | 6 4 9 |
+-------+-------+-------+
| 5 3 9 | 1 8 6 | 7 2 4 |
| 2 4 8 | 9 3 7 | 1 5 6 |
| 7 1 6 | 5 4 2 | 3 9 8 |
+-------+-------+-------+
+-------+-------+-------+
|     5 | 7 6   |     2 |
| 4     | 2     |       |
|       |       | 8 7   |
+-------+-------+-------+
|       |   5   |     7 |
|   8   |   2 4 |     1 |
|   5   |     1 |   4   |
+-------+-------+-------+
|     9 |       |   2   |
| 2     |       |   5   |
|     6 |       | 3   8 |
+-------+-------+-------+
Generated sudoku is minimal

If we eliminate each hint at a time, we always get more than one solutions:

  example0 :: Grid
  example0 = [[0,0,5,7,6,0,0,0,2],
              [4,0,0,2,0,0,0,6,0],
              [0,0,0,0,0,0,8,7,0],
              [0,0,0,0,5,0,0,0,7],
              [0,8,0,0,2,4,0,0,1],
              [0,5,0,0,0,1,0,4,0],
              [0,0,9,0,0,0,0,2,0],
              [2,0,0,0,0,0,0,5,0],
              [0,0,6,0,0,0,3,0,8]]

Example: (1,3) -> 0. If we run solveAndShow example0 we get 11 diff solutions.

-}



  main :: IO ()
  main = do [r] <- rsolveNs [emptyN]
            showNode r
            s  <- genProblem r
	--    showSudoku (fst s)
	    showNode s
	    if (checkMinimal s (filledPositions (fst s)))
	--  if (checkMinimal s )
		then putStrLn ("Generated sudoku is minimal")
		else putStrLn ("Generated sudoku is not minimal")

  checkMinimal :: Node -> [(Row,Column)] -> Bool
  checkMinimal x [] = True
  checkMinimal x (y:ys) = if (not $ uniqueSol $ eraseN x y)
				then checkMinimal x ys
				else False

