> module Exercise3 where

> import Lecture3

> cnf :: Form -> Form
> cnf = (dist . nnf . arrowfree)

> dist :: Form -> Form
> dist (Prop p) = Prop p
> dist (Neg (Prop p)) = Neg (Prop p)
> dist (Cnj fs) = Cnj (map dist fs)   
> dist (Dsj fs) = Dsj (map dist fs)
> dist (Dsj [Cnj [f1, f2], f3]) = Cnj [ dist (Dsj[dist f1, dist f3]), (Dsj[dist f2, dist f3])]
> dist (Dsj [f1, Cnj [f2, f3]]) = Cnj [ dist (Dsj[dist f1, dist f2]), (Dsj[dist f2, dist f3])]