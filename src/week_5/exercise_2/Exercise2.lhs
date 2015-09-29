> module Exercise2 where


> import Lecture5

> type Position = (Row,Column)
> type Constrnt = [[Position]]


New Style constraints


> rowConstrnt = [[(r,c)| c <- values ] | r <- values ]


> columnConstrnt = [[(r,c)| r <- values ] | c <- values ]


> blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]



> freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
> freeAtPos' s (r,c) xs = let 
>    ys = filter (elem (r,c)) xs 
>  in 
>    foldl1 intersect (map ((values \\) . map s) ys)