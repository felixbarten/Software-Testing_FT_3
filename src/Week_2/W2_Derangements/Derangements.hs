module Week_2.W2_Derangements.Derangements where

import Data.List
import System.Random
import Week_2.Testing

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = deran xs == deran ys

deran :: Eq a => [a] -> [[a]]
deran xs =  filter (and . zipWith (/=) xs) $ permutations xs

{-
blah :: [a] -> [[a]]
blah xs = deran_own permutations xs
-}
{-
deran_own :: Eq a => [a] -> [[a]]
deran_own [] = []
deran_own (x:xs) =  if x /= head (head ls) then [x] : deran_own xs else deran_own xs
    where ls = permutations (x:xs)
   -}

deran_own :: [a] -> [[a]]
deran_own [] = [[]]
deran_own (x:xs) =  do 
                    ls <- permutations xs 
                    if x /= (head (head ls)) then [x] : deran_own xs else deran_own xs
                    
                    