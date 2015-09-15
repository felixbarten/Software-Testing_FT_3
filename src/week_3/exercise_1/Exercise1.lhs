> module Exercise1 where


> import Lecture3

> contradiction :: Form -> Bool
> contradiction f = not(satisfiable f)

> tautology :: Form -> Bool
> tautology f = all (\ v -> evl v f) (allVals f)

{-	all contradictions are false 	

	tautology form1 = true 
	tautology form2 = false
	tautology form3 = true - }


{-
 > entails:: Form -> Form -> Bool
 > entails x z = all (\ n -> evl n x) ((x:xs) -> (evl xs z)) 

 shit

entails :: Form -> Form -> Bool
--entails f g = all (\n -> evl n g) (\x -> evl x f)
entails f g = all (\n -> evl n g: evl n f) (allVals f)


-- gia kathe x p ikanopoiei thn f, synepagetai g(x).

-}
