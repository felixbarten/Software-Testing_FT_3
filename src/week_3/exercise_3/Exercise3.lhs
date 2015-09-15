> module Exercise3 where

> import Lecture3


> algDistribution:: Form -> Form
> algDistribution(Prop x) = Prop x
> algDistribution(Neg (Prop x)) = Neg (Prop x)

> algDistribution(Dsj [f1,Cnj[ f2, f3]]) = Cnj[algDistribution (Dsj[algDistribution f1,algDistribution f2]),algDistribution (Dsj[algDistribution f1,algDistribution f3])]

> algDistribution(Dsj [Cnj[ f1, f2], f3]) = Cnj[algDistribution (Dsj[algDistribution f1,algDistribution f3]),algDistribution (Dsj[algDistribution f2,algDistribution f3])]

> algDistribution(Cnj fs) = Cnj (map algDistribution fs)
> algDistribution(Dsj fs) = Dsj (map algDistribution fs)

> convertToCNF:: Form -> Form
> convertToCNF x = ( algDistribution . nnf . arrowfree ) x
