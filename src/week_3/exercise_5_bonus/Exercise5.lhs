> module Exercise5 where

> import Exercise3
> import Lecture3

> data Clause = Clause [Int]
> type Clauses = [Clause]

 data Clauses = Clause [Int]
           | [Clause]
           deriving Eq

> instance Show Clause where 
>   show (Clause xs)   = show xs




> cnf2cls :: Form -> Clauses
> cnf2cls = undefined

> cls :: Form -> Clauses
> cls (Prop x) = [(Clause [x])]
> cls (Neg (Prop x)) = [(Clause [-x])]

 cls (Cnj [f1, f2]) = cls f1 ++ cls f2
 cls (Dsj [f1, f2]) =   [Clause [head $ read $ show (cls f1 ++ cls f2)]]
 
 
 cls (Cnj [f1,f2]) = cls f1 ++ cls f2
 
cls (Cnj fs) =   [(Clause  [concat $ map cls fs])]

> cls (Dsj fs) = [(Clause [concat $ head (map cls fs)])]
 
> cls (Impl _ _) = []
> cls (Equiv _ _) = []




> cnf2cls2 :: Form -> Clauses
> cnf2cls2 f = [cls2 f]



> cls2 :: Form -> Clause
> cls2 (Prop x) = (Clause [x])
> cls2 (Neg (Prop x)) = (Clause [-x])
> cls2 (Dsj fs) =   (Clause  [map cls2 fs])


 cls (Cnj [f1, f2]) = cls f1 ++ cls f2
 cls (Dsj [f1, f2]) =   [Clause [head $ read $ show (cls f1 ++ cls f2)]]
 
 
 cls (Cnj [f1,f2]) = cls f1 ++ cls f2
 

 cls (Dsj fs) = Clause [concat $ head (map cls fs)]
 



























Examples from lab desc.

> clsform1 = p
> clsform2 = Neg (p) 
> clsform3 = Dsj [p, (Neg q)]
> clsform4 = Cnj [r, Dsj [p,Neg (q)]]




> mergeTwo :: Ord a => [a] -> [a] -> [a]
> mergeTwo x [] = x
> mergeTwo [] x = x
> mergeTwo (x:xs) (y:ys) = if x < y
>                          then x:(mergeTwo xs (y:ys))
>                          else y:(mergeTwo (x:xs) ys)

> mergePairs :: Ord a => [[a]] -> [[a]]
> mergePairs [] = []
> mergePairs (x:[]) = [x]
> mergePairs (x:y:tail) = mergePairs ((mergeTwo x y):(mergePairs tail))

> mergeAll :: Ord a => [[a]] -> [a]
> mergeAll [] = []
> mergeAll x = head $ mergePairs x

 basdf (Dsj [f1, f2]) = basdf f1 ++ basdf f2

 basdf (Dsj [f1, f2]) = concat (basdf f1 ++ basdf f2)

 basdf (Cnj fs) =  [Clause (map basdf fs)]
 basdf (Dsj fs) =  [Clause (map basdf fs)]


 getClause :: Form -> Form -> Clause
 getClause f1 f2 = Clause [( head ( cls f1)) ++ (head (cls f2))]

 t1 :: Clause
 t1 fs = Clause [foldr (\x acc -> (head . cls) x) 0 fs]

> getClause :: Form -> Form -> Clause
> getClause f1 f2 = Clause getLiterals

> getLiterals :: [Int]
> getLiterals = [1,2,3,4]

Time spent attempting to complete the exercise: at least two hours.