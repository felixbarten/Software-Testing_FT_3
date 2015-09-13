module Week_2.W2_Triangles.Triangles where

import  Week_2.Testing

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | isEquilateral x y z = Equilateral
               | isIsosceles x y z  = Isosceles
               | isRectangular x y z = Rectangular
               | isNoTriangle x y z = NoTriangle
               | isOther x y z = Other
-- ^ could have been written as one function
                 
isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral x y z = x == y && y == z

isIsosceles  :: Integer -> Integer -> Integer -> Bool
isIsosceles x y z = (x == y || x ==z || y == z) && not ( x == y && x == z && y ==z)

isRectangular  :: Integer -> Integer -> Integer -> Bool
isRectangular x y z = x^2 + y^2 == z^2

isNoTriangle  :: Integer -> Integer -> Integer -> Bool
isNoTriangle a b c = not (a + b > c && a + c > b && b + c > a)

isOther :: Integer -> Integer -> Integer -> Bool
isOther x y z =  not (isEquilateral x y z) && not ( isIsosceles x y z ) && not  (isRectangular x y z) && not (isNoTriangle x y z)
{- 
solution written in about 30 minutes, had some trouble with conflicting definitions of triangle inequality 
-}

testRectangular, testIsosceles, testEquilateral, testNoTriangle, testIsOther :: (Integer, Integer,Integer, Shape) -> Bool

testRectangular(x,y,z,shape) = triangle x y z == shape
testIsosceles (x,y,z,shape) = triangle x y z == shape
testEquilateral (x,y,z,shape) = triangle x y z == shape
testNoTriangle (x,y,z,shape) = triangle x y z == shape
testIsOther (x,y,z,shape) = triangle x y z == shape

rectangularTests :: [Test]
rectangularTests = [Test "Rectangular Tests" testRectangular [(200, 375, 425, Rectangular), (12, 5,13, Rectangular)]
           ]

isoscelesTests :: [Test]
isoscelesTests = [Test "Isosceles Tests" testIsosceles [(5, 5, 9, Isosceles), (5, 8,8, Isosceles)]
           ]

isNoTriangleTests :: [Test]
isNoTriangleTests = [Test "NoTriangle Tests" testNoTriangle [(5, 3, 9, NoTriangle), (5, 3,8, NoTriangle)]
           ]
-- test for scalene triangles
isOtherTests :: [Test]
isOtherTests = [ Test "Other Tests" testIsOther [(13, 14, 9, Other), (15, 14,9, Other)]
           ]

equilateralTests :: [Test]
equilateralTests = [Test "Rectangular Tests" testEquilateral [(350, 350, 350, Equilateral), (12, 12,12, Equilateral)]
           ]          

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ rectangularTests
                  , equilateralTests
                  , isoscelesTests
                  , isNoTriangleTests
                  , isOtherTests
                  ]