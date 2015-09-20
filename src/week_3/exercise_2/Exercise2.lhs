> module Exercise2 where

> import Data.List
> import System.Random

> import Test.QuickCheck

> import Lecture3

> testParse :: String -> Bool
> testParse x = parse x  /= []


*Exercise2> testParse "*(1 +(2 -3)"
False
*Exercise2> testParse "(1 +(2 -3))"
False
*Exercise2> testParse "*(1 +(2 -3))"
True
*Exercise2>