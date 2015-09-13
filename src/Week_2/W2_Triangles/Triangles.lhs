> module Week_2.W2_Triangles.Triangles where

> import Data.List
> import Data.Char
> import System.Random
> import Testing

Test Report
==========

 The triangle function is tested using values which conform to a triangle whose type was already known. One of the tests using a function that generates
 Pythagorean triples which are used to the functions interpretation of Rectangular(Right-angled?) triangles

 triangle 0 1 2 = NoTriangle == Correct
 triangle 2 2 3 = Isocele == Correct
 triangle 1 1 1 = Equilateral == Correct
 triangle 2 6 8 = Other == Correct

*Time spent: 1 hours*

Define the different types of triangles

> data Shape = NoTriangle | Equilateral 
>            | Isosceles  | Rectangular | Other deriving (Eq,Show)

 Triangle stuff

> triangle :: Integer -> Integer -> Integer -> Shape
> triangle x y z | not (x+z >= y && x+y >= z && z+y >= x) || x == 0 || y == 0 || z == 0 = NoTriangle 
>                | x == y && x == z = Equilateral
>                | x == y || x == z || y == z = Isosceles
>                | x^2 + y^2 == z^2 || x^2 + z^2 == y^2 || y^2 + z^2 == x^2 = Rectangular
>                | otherwise = Other

> pythagoreanTriples :: Integer -> [(Integer, Integer, Integer)]
> pythagoreanTriples n = [( x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x <= y, (x^2) + (y^2) == (z^2) ]

> testRightTriangle :: Bool 
> testRightTriangle = all (\(x,y,z) -> triangle x y z == Rectangular ) (pythagoreanTriples 50)

Tests 

> testTriangle :: ((Integer,Integer,Integer), Shape) -> Bool
> testTriangle ((x, y, z), t) = triangle x y z == t

> triangleTests :: [Test]
> triangleTests = [ Test "triangle Test" testTriangle [((0,1,3),NoTriangle), ((2,2,3), Isosceles), ((1,1,1), Equilateral ), ((2,6,8), Other)]]



