> module Exercise1 where


> import Lecture5


> example6 :: Grid
> example6 = [[5,3,0,0,7,0,0,0,0],
>             [6,0,0,1,9,5,0,0,0],
>             [0,9,8,0,0,0,0,6,0],
>             [8,0,0,0,6,0,0,0,3],
>             [4,0,0,8,0,3,0,0,1],
>             [7,0,0,0,2,0,0,0,6],
>             [0,6,0,0,0,0,2,8,0],
>             [0,0,0,4,1,9,0,0,5],
>             [0,0,0,0,8,0,5,7,9]]

NRC example:

> exampleCons :: Grid
> exampleCons = 
>             [[0,0,0,3,0,0,0,0,0],
>             [0,0,0,7,0,0,3,0,0],
>             [2,0,0,0,0,0,0,0,8],
>             [0,0,6,0,0,5,0,0,0],
>             [0,9,1,6,0,0,0,0,0],
>             [3,0,0,0,7,1,2,0,0],
>             [0,0,0,0,0,0,0,3,1],
>             [0,8,0,0,4,0,0,0,0],
>             [0,0,2,0,0,0,0,0,0]]


Desired NRC sudoku output:
             +---------+---------+---------+
             |         | 3       |         |
             |   +-----|--+   +--|-----+   |
             |   |     | 7|   |  | 3   |   |
             | 2 |     |  |   |  |     | 8 |
             +---------+---------+---------+
             |   |   6 |  |   |5 |     |   |
             |   +-----|--+   +--|-----+   |
             |    9  1 | 6       |         |
             |   +-----|--+   +--|-----+   |
             | 3 |     |  | 7 |1 | 2   |   |
             +---------+---------+---------+
             |   |     |  |   |  |    3| 1 |
             |   |8    |  | 4 |  |     |   |
             |   +-----|--+   +--|-----+   |
             |       2 |         |         |
             +---------+---------+---------+


Special row dividers for boundaries of sub grids.

> showSubGridRow :: IO()
> showSubGridRow = do putStr ("|   +-----|--+   +--|-----+   |")  ;putChar '\n';

ShowRow function for showing values which are not in a internal block (row 1, 5 and 9)

> showRow'' :: [Value] -> IO()
> showRow'' [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
>  do  putChar '|'         ; putChar ' '
>      putStr (showVal a1) ; putChar ' '; putChar ' '
>      putStr (showVal a2) ; putChar ' '; putChar ' '
>      putStr (showVal a3) ; putChar ' '
>      putChar '|'         ; putChar ' '
>      putStr (showVal a4) ; putChar ' '; putChar ' '
>      putStr (showVal a5) ; putChar ' '; putChar ' '
>      putStr (showVal a6) ; putChar ' '
>      putChar '|'         ; putChar ' '
>      putStr (showVal a7) ; putChar ' '; putChar ' '
>      putStr (showVal a8) ; putChar ' '; putChar ' '
>      putStr (showVal a9) ; putChar ' '
>      putChar '|'         ; putChar '\n'


> showRow' :: [Value] -> IO()
> showRow' [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
>  do  putChar '|'         ; putChar ' '
>      putStr (showVal a1) ; putChar ' '
>      putChar '|'         ;
>      putStr (showVal a2) ; putChar ' '; putChar ' '
>      putStr (showVal a3) ; putChar ' '
>      putChar '|'         ; putChar ' '
>      putStr (showVal a4) ;
>      putChar '|'         ; putChar ' '
>      putStr (showVal a5) ; putChar ' '
>      putChar '|'         ;
>      putStr (showVal a6) ; putChar ' '
>      putChar '|'         ; putChar ' '
>      putStr (showVal a7) ; putChar ' '; putChar ' '
>      putStr (showVal a8) ;
>      putChar '|'         ; putChar ' '
>      putStr (showVal a9) ; putChar ' '
>      putChar '|'         ; putChar '\n'

Showing a grid, i.e., a sequence of rows.


> showGrid' :: Grid -> IO()
> showGrid' [as,bs,cs,ds,es,fs,gs,hs,is] =
>  do putStrLn ("+---------+---------+---------+")
>     showRow'' as;
>     showSubGridRow; 
>     showRow' bs; showRow' cs
>     putStrLn ("+---------+---------+---------+")
>     showRow' ds; 
>     showSubGridRow;
>     showRow'' es; 
>     showSubGridRow;
>     showRow' fs
>     putStrLn ("+---------+---------+---------+")
>     showRow' gs; showRow' hs; 
>     showSubGridRow; 
>     showRow'' is
>     putStrLn ("+---------+---------+---------+")


Plug new show function in existing generator...

> showNode' :: Node -> IO()
> showNode' = showSudoku' . fst


> showSudoku' :: Sudoku -> IO()
> showSudoku' = showGrid' . sud2grid

adapted main for NRC 

It is not recommended to try the main' function! 
It is likely to generate a problem that doesn't adhere to the new constraints and therefore will never be solved 

> main' :: IO ()
> main' = do [r] <- rsolveNs [emptyN]
>            showNode' r
>            s  <- genProblem r
>            showNode' s



New consistancy func for NRC sudoku

> consistent' :: Sudoku -> Bool
> consistent' s = and $
>                [ rowInjective s r |  r <- positions ]
>                 ++
>                [ colInjective s c |  c <- positions ]
>                 ++
>                [ subgridInjective s (r,c) | 
>                     r <- [1,4,7], c <- [1,4,7]]
>                ++
>                [ subgridInjective s (r,c) | 
>                     r <- [2,6], c <- [2,6]]

A problem with the new blocks is that they are inside the existing blocks so they need to be able to be detected as such. 
Otherwise a possible solution might be rejected 

Solving and showing the results with new functions

> solveAndShow' :: Grid -> IO[()]
> solveAndShow' gr = solveShowNs' (initNode' gr)
> 
> solveShowNs' :: [Node] -> IO[()]
> solveShowNs' = sequence . fmap showNode' . solveNs


> initNode' :: Grid -> [Node]
> initNode' gr = let s = grid2sud gr in 
>               if (not . consistent') s then [] 
>               else [(s, constraints s)]

How to use: 

*Exercise1> solveAndShow' exampleCons 
+---------+---------+---------+
| 4   7 8 | 3  9  2 | 6 1   5 |
|   +-----|--+   +--|-----+   |
| 6 | 1 9 | 7| 5 |8 | 3 2 | 4 |
| 2 | 3 5 | 4| 1 |6 | 9 7 | 8 |
+---------+---------+---------+
| 7 | 2 6 | 8| 3 |5 | 1 4 | 9 |
|   +-----|--+   +--|-----+   |
| 8   9 1 | 6  2  4 | 7 5   3 |
|   +-----|--+   +--|-----+   |
| 3 | 5 4 | 9| 7 |1 | 2 8 | 6 |
+---------+---------+---------+
| 5 | 6 7 | 2| 8 |9 | 4 3 | 1 |
| 9 | 8 3 | 1| 4 |7 | 5 6 | 2 |
|   +-----|--+   +--|-----+   |
| 1   4 2 | 5  6  3 | 8 9   7 |
+---------+---------+---------+
[()]
