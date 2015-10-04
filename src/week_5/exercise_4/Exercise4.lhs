> module Exercise4 where

Report 
================

It is possible to generate sudoku puzzles with 3 and even 4 empty blocks but there are issues generating ones with 5 due to lack of unrelated blocks


*Time spent: 1 hour*


> import Lecture5 hiding (genProblem, main)


> blocks' = do 
>            b <- randomize [(x,y) | x <- blocks, y <- blocks]
>            return b 

> eraseBlocks :: Node -> [(Row,Column)] -> Node
> eraseBlocks n [] = n
> eraseBlocks n (p:ps) = eraseBlocks (eraseN n p) ps

> genProblem :: Node -> [(Row,Column)] -> IO Node
> genProblem n bs = do ys <- randomize xs
>                      return (minimalize n' ys)
>    where xs = filledPositions (fst n)
>          n' = (eraseBlocks n bs)

> pickB x xs = concat $ take x xs
> reorder = map (\(x,y) -> [(a,b)| a <- x, b <- y ])   

  Provide n to indicate how many blocks should be empty

> main :: Int -> IO ()
> main n = do 
>           [r] <- rsolveNs [emptyN]
>           showNode r             
>           b <- blocks'
>           s <- genProblem r (pickB  n (reorder b)) 
>           showNode s

