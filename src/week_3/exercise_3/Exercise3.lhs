> module Exercise3 where

> import Lecture3


Documentation
============

To convert any form into its cnf equivalent use the toCnf function

Time spent: 2 hours

> toCnf :: Form -> Form
> toCnf = simplify . cnf . nnf . arrowfree

Assumes that formula is arrowfree and nnf

> cnf :: Form -> Form
> cnf (Prop p) = Prop p
> cnf (Neg (Prop p)) = Neg (Prop p)
> cnf (Cnj fs) = Cnj (map cnf fs)   
> cnf (Dsj []) = Dsj []
> cnf (Dsj [f]) = cnf f 
> cnf (Dsj (f1:f2:fs)) = distCnjOverDsj (cnf f1) (cnf (Dsj(f2:fs)))
> cnf f = f  

  
This function uses the distribution law to distribute conjunctions over disjunctions

> distCnjOverDsj :: Form -> Form -> Form 
> distCnjOverDsj (Cnj []) _ = Cnj []
> distCnjOverDsj (Cnj [f1]) f2 = distCnjOverDsj f1 f2
> distCnjOverDsj (Cnj (f1:fs)) f2 = Cnj [distCnjOverDsj f1 f2, distCnjOverDsj (Cnj fs) f2]
> distCnjOverDsj _ (Cnj []) = Cnj []
> distCnjOverDsj f1 (Cnj [f2]) = distCnjOverDsj f1 f2
> distCnjOverDsj f1 (Cnj (f2:fs)) = Cnj [distCnjOverDsj f1 f2, distCnjOverDsj f1 (Cnj fs)]
> distCnjOverDsj f1 f2 = Dsj [f1,f2]

This is used to simplify the end result by rewriting conjunctions of conjunctions and disjunctions of disjunctions(also removes duplicates). Assummes that formula is in cnf

> simplify :: Form -> Form
> simplify (Dsj fs) =  Dsj (nub $ flattenDsj fs)
> simplify (Cnj fs) = aux $ Cnj (flattenCnj fs)
>                       where aux (Dsj fs) = Dsj (nub fs)
>                             aux f  = f

Homemade nub used in simplify 

> nub :: Eq a => [a] -> [a]
> nub [] = []
> nub (x:xs) = x : nub (filter (/= x) xs)


Rewrites disjunctions of disjunctions into one

> flattenDsj :: [Form] -> [Form]
> flattenDsj [] = []
> flattenDsj ((Dsj fs):gs) = flattenDsj (fs ++ gs)
> flattenDsj (f:fs) = f: flattenDsj fs


Rewrites conjunctions of conjunctions into one 

> flattenCnj :: [Form] -> [Form] 
> flattenCnj [] = []
> flattenCnj ((Cnj fs):gs) = flattenCnj (fs ++ gs)
> flattenCnj (f:fs) = simplify f : flattenCnj fs
 

