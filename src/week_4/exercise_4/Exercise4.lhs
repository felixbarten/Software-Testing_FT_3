> module Exercise4 where
> import System.Random


> divisors :: Integer -> [(Integer,Integer)]
> divisors n = [ (d, quot n d) | d <- [1..k], rem n d == 0 ]
> 	where k = floor (sqrt (fromInteger n))


Exercise 5.13 Show that ∀x ∀y ∃R (xRy). (“Between every two things there exist
some relation.”)

totalR

pdf --> sel_192


exerc_5.56
