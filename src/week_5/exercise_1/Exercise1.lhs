> module Exercise1 where


> import Lecture5


Define additional subgrids 

2,2 2,6
6,2 ,6,6

probably new blocks function, and check injectivity for those, maybe possible to impl. in existing blocks function

start with show?


> exampl201 :: Grid
> exampl201 = [[5,3,0,0,7,0,0,0,0],
>             [6,0,0,1,9,5,0,0,0],
>             [0,9,8,0,0,0,0,6,0],
>             [8,0,0,0,6,0,0,0,3],
>             [4,0,0,8,0,3,0,0,1],
>             [7,0,0,0,2,0,0,0,6],
>             [0,6,0,0,0,0,2,8,0],
>             [0,0,0,4,1,9,0,0,5],
>             [0,0,0,0,8,0,5,7,9]]


Showing a row by sending it to the screen; not the type IO() for the result:

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


> showSubGridRow :: IO()
> showSubGridRow = do putStr ("|   +-----|--+   +--|-----+   |")  ;putChar '\n';


> showRow'' :: [Value] -> IO()
> showRow'' [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
>  do  putChar '|'         ; putChar ' '
>      putStr (showVal a1) ; putChar ' '; putChar ' '; putChar ' '
>      putStr (showVal a2) ; putChar ' '
>      putStr (showVal a3) ; putChar ' '
>      putChar '|'         ; putChar ' '
>      putStr (showVal a4) ; putChar ' '; putChar ' '
>      putStr (showVal a5) ; putChar ' '; putChar ' '
>      putStr (showVal a6) ; putChar ' '
>      putChar '|'         ; putChar ' '
>      putStr (showVal a7) ; putChar ' '
>      putStr (showVal a8) ; putChar ' '; putChar ' '; putChar ' '
>      putStr (showVal a9) ; putChar ' '
>      putChar '|'         ; putChar '\n'


> showRow' :: [Value] -> IO()
> showRow' [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
>  do  putChar '|'         ; putChar ' '
>      putStr (showVal a1) ; putChar ' '
>      putChar '|'         ; putChar ' '
>      putStr (showVal a2) ; putChar ' '
>      putStr (showVal a3) ; putChar ' '
>      putChar '|'         ; putChar ' '
>      putStr (showVal a4) ;
>      putChar '|'         ; putChar ' '
>      putStr (showVal a5) ; putChar ' '
>      putChar '|'         ;
>      putStr (showVal a6) ; putChar ' '
>      putChar '|'         ; putChar ' '
>      putStr (showVal a7) ; putChar ' '
>      putStr (showVal a8) ; putChar ' '
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

> main' :: IO ()
> main' = do [r] <- rsolveNs [emptyN]
>            showNode' r
>            s  <- genProblem r
>            showNode' s
