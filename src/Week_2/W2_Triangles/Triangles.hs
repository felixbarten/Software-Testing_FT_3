module Week_2.W2_Triangles.Triangles where

import Testing

data Shape = NoTriangle | Equilateral
		| Isosceles |Rectangular | Other deriving(Eq,Show)


triangle:: Integer -> Integer -> Integer -> Shape
triangle x y z  | equilateral x y z = Equilateral
		| isosceles x y z = Isosceles
		| rectangular x y z = Rectangular
		| noTriangle x y z = NoTriangle
		| otherwise = Other

equilateral:: Integer -> Integer -> Integer -> Bool
equilateral x y z = x == y && y == z

isosceles:: Integer -> Integer -> Integer -> Bool
isosceles x y z = x == y || x == z || y == z

rectangular:: Integer -> Integer -> Integer -> Bool
rectangular x y z = x*x + y*y == z*z || x*x + z*z == y*y || y*y + z*z == x*x

noTriangle:: Integer -> Integer -> Integer -> Bool
noTriangle x y z = x+y < z || x+z < y || y+z < x


--------- test triangle:  runTests trianglesTest --------------

testTriangle:: (Integer, Integer , Integer ,Shape) -> Bool
testTriangle (x, y, z, s)=  triangle x y z == s

trianglesTests:: [Test]
trianglesTests =[ Test "testTriangle" testTriangle
			[(3,4,5,Rectangular),(6,6,6,Equilateral),(5,5,7,Isosceles),
			(3,4,10,NoTriangle), (2,3,4,Other)]
		]
