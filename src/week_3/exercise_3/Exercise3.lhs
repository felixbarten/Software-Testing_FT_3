> module Exercise3 where

> import Lecture3
> import Data.List
> import Data.Char


> algDistribution:: Form -> Form
> algDistribution(Prop x) = Prop x
> algDistribution(Neg (Prop x)) = Neg (Prop x)

> algDistribution(Dsj [f1,Cnj[ f2, f3]]) = Cnj[algDistribution (Dsj[algDistribution f1,algDistribution f2]),algDistribution (Dsj[algDistribution f1,algDistribution f3])]

> algDistribution(Dsj [Cnj[ f1, f2], f3]) = Cnj[algDistribution (Dsj[algDistribution f1,algDistribution f3]),algDistribution (Dsj[algDistribution f2,algDistribution f3])]

> algDistribution(Cnj fs) = Cnj (map algDistribution fs)
> algDistribution(Dsj fs) = Dsj (map algDistribution fs)

***** Convert to CNF form below ******

> convertToCNF:: Form -> Form
> convertToCNF x = ( algDistribution . nnf . arrowfree ) x


**** explicit forms have been checked ****
**** postcondition 1: form does not contain implications or equivalences ****

> testLexer:: String -> Bool

> testLexer [] = True
> testLexer (c:cs) | isSpace c = testLexer cs
>              | isDigit c = testLexer (c:cs) 
> testLexer ('(':cs) =  testLexer cs
> testLexer (')':cs) =  testLexer cs
> testLexer ('*':cs) =  testLexer cs
> testLexer ('+':cs) =  testLexer cs
> testLexer ('-':cs) =  testLexer cs 
> testLexer ('=':'=':'>':cs) = False
> testLexer ('<':'=':'>':cs) = False
> testLexer (x:cs) = testLexer cs
