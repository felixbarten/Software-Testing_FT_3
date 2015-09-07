module Week_2.W2_Triangles.Triangles where

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | x == y && y == z = Equilateral
               |(x == y || x ==z || y == z) && not ( x == y && x == z && y ==z) = Isosceles
               | x^2 + y^2 == z^2 = Rectangular
               | isNoTriangle x y z = NoTriangle
               | isOther x y z = Other
                 
isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral x y z = x == y && y == z

isIsosceles  :: Integer -> Integer -> Integer -> Bool
isIsosceles x y z = (x == y || x ==z || y == z) && not ( x == y && x == z && y ==z)

isRectangular  :: Integer -> Integer -> Integer -> Bool
isRectangular x y z = x^2 + y^2 == z^2

isNoTriangle  :: Integer -> Integer -> Integer -> Bool
isNoTriangle a b c = not (a+b > c && a+c > b && b+c>a)

isOther :: Integer -> Integer -> Integer -> Bool
isOther x y z =  not (isEquilateral x y z) && not ( isIsosceles x y z ) && not  (isRectangular x y z) && not (isNoTriangle x y z)
{- 
solution written in about 30 minutes, had some trouble with conflicting definitions of triangle inequality 
-}
