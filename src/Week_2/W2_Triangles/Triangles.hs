module Week_2.W2_Triangles.Triangles where

import Data.List
import Data.Char
import System.Random

-- Define the different types of triangles

data Shape = NoTriangle | Equilateral 
            | Isosceles  | Rectangular | Other deriving (Eq,Show)

-- Triangle stuff
triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | not (x+z >= y && x+y >= z && z+y >= x) || x == 0 || y == 0 || z == 0 = NoTriangle 
               | x == y && x == z = Equilateral
               | x == y || x == z || y == z = Isosceles
               | x^2 + y^2 == z^2 || x^2 + z^2 == y^2 || y^2 + z^2 == x^2 = Rectangular
               | otherwise = Other

pythagoreanTriples :: Integer -> [(Integer, Integer, Integer)]
pythagoreanTriples n = [( x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x <= y, (x^2) + (y^2) == (z^2) ]

testRightTriangle :: Bool 
testRightTriangle = all (\(x,y,z) -> triangle x y z == Rectangular ) (pythagoreanTriples 50)
--[(x,y,z) | x <- [1..n]ky, y <- [1..n], z <- [1..n], x^2 + ]


-- Report
-- The function was tested with values which conform to a triangle whose type was already known.

-- triangle 0 1 2 = NoTriangle == Correct
--
-- triangle 2 2 3 = Isocele == Correct
--
-- triangle 1 1 1 = Equilateral == Correct
--
-- triangle 2 6 8 = Other == Correct

-- It took about an hour and thirty minutes



