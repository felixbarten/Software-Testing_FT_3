> module Exercise1 where
  
>  import Data.List
>  import System.Random
>  import Lecture5

** New functions in Lecture5.lhs: internalBlocks, sameInternalBlock, internalbl -- line 300 -- **

** Function prune in Lecture5 has been edited **

*** Result is shown below ***
*** Time spent: 1h ***

*Ex_1> solveAndShowNRC example0

+-------+-------+-------+
| 4 7 8 | 3 9 2 | 6 1 5 |
| 6 1 9 | 7 5 8 | 3 2 4 |
| 2 3 5 | 4 1 6 | 9 7 8 |
+-------+-------+-------+
| 7 2 6 | 8 3 5 | 1 4 9 |
| 8 9 1 | 6 2 4 | 7 5 3 |
| 3 5 4 | 9 7 1 | 2 8 6 |
+-------+-------+-------+
| 5 6 7 | 2 8 9 | 4 3 1 |
| 9 8 3 | 1 4 7 | 5 6 2 |
| 1 4 2 | 5 6 3 | 8 9 7 |
+-------+-------+-------+


>  nrcConsistent :: Sudoku -> Bool
>  nrcConsistent s = and $
>                [ rowInjective s r |  r <- positions ]
>                 ++
>                [ colInjective s c |  c <- positions ]
>                 ++
>                [ subgridInjective s (r,c) | 
>                     r <- [1,4,7], c <- [1,4,7]]
>		  ++
>		 [ subgridInjective s (r,c) | 
>                     r <- [2,6], c <- [2,6]]


>  initNRC :: Grid -> [Node]
>  initNRC gr = let s = grid2sud gr in 
>               if (not . nrcConsistent) s then [] 
>               else [(s, constraints s)]


>  solveAndShowNRC :: Grid -> IO[()]
>  solveAndShowNRC gr = solveShowNs (initNRC gr)
