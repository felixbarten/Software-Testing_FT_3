> module Exercise7 where

> import qualified Regular as Reg
> import qualified Nrc
> import qualified Exercise1 as Exc1
> import Data.List
> import Control.Monad

Returns amount of hints in regular sudoku

> regularHints :: IO Int
> regularHints =  do   [r] <- Reg.rsolveNs [Reg.emptyN]
>                      s <- Reg.genProblem r
>                      let len = length $ Reg.filledPositions $ fst s
>                      return len

Returns amount of hints in nrc sudoku

> nrcHints :: IO Int
> nrcHints = do   
>               [r] <- Nrc.rsolveNs [Nrc.emptyN]
>               s  <- Nrc.genProblem r
>               let len = length $ Nrc.filledPositions $ fst s
>               return len


> getHints :: Int -> (IO Int) ->  IO Int
> getHints i f = do
>                   hints <- replicateM i f
>                   let sumh = sum hints
>                   return sumh

> avgHints :: Int -> IO () 
> avgHints n = do 
>               regHints <- getHints n regularHints
>               nrcHints <- getHints n nrcHints
>               let avgReg =  fromIntegral  (regHints) / fromIntegral (n)
>               let avgNrc =  fromIntegral (nrcHints) / fromIntegral (n)
>               print ("Average regular hints: " ++ show avgReg ++ " Average NRC hints: " ++ show avgNrc)





Minimal problems for NRC Sudokus need fewer hints than standard Sudoku problems. 
Investigate the difference. What is the average number of hints in a minimal standard Sudoku problem? What is the average number of hints in a minimal NRC Sudoku problem?

Output from code: 

*Exercise7> avgHints 10
"Average regular hints: 25 Average NRC hints: 16.7"
*Exercise7>

Unfortunately my implementation of 5 is not really fast so the computation of the averages takes a while. Therefore the sample size is kept small (10)

Time spent: 1 hour
