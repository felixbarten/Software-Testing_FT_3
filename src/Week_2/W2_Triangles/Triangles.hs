module Week_2.W2_Triangles.Triangles where

data Shape = NoTriangle | Equilateral
		| Isosceles |Rectangular | Other deriving(Eq,Show)


triangle:: Integer -> Integer -> Integer -> Shape
triangle x y z | x == y && y == z = Equilateral
		| x == y || x == z || y == z = Isosceles
		| x*x + y*y == z*z || x*x + z*z == y*y || y*y + z*z == x*x = Rectangular
		| x+y < z || x+z < y || y+z < x = NoTriangle
		| otherwise = Other
