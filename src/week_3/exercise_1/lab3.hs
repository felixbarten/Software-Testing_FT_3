import Lecture3

contradiction :: Form -> Bool
contradiction f = not(satisfiable f)

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

entails :: Form -> Form -> Bool
--entails f g = all (\n -> evl n g) (\x -> evl x f)
entails f g = all (\n -> evl n g: evl n f) (allVals f)
--entails f g = (\ v -> if (evl v f) then (evl v g) else False) (allVals f) 


-- gia kathe x p ikanopoiei thn f, synepagetai g(x).



-- exercise 2 ------------------
