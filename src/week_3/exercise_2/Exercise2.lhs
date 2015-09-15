> module Exercise2 where

Testing module for Lecture3 Parse merthod

> import Lecture3
> import Test.QuickCheck


Going to shelf this for now...
             a <- sized $ \ size -> arbitrary
             b <- sized $ \ size -> arbitrary


> instance Arbitrary Form where
>   arbitrary   = sized form



> form :: Int -> Gen Form 
> form 0 = do
>       x <- choose(1,10) :: Gen Int 
>       return (Prop x)
> form n | n > 0= do
>       x <- choose (1,10) :: Gen Int
>       y <- choose (1,10) :: Gen Int
>       p <- choose (1,10) :: Gen Int
>       choice <- choose (0,4) :: Gen Int
>       case (choice) of
>           0 -> return $ Prop x
>           1 -> do 
>               f2 <- listOf subform
>               return $ Cnj $ f2
>           2 -> do 
>               f2 <- listOf subform
>               return $ Dsj $ f2
>           3 -> do
>               f1 <- subform
>               f2 <- subform
>               return $ Impl (f1) (f2) 
>           4 -> do
>               f1 <- subform
>               f2 <- subform
>               return $ Equiv (f1) (f2)
>       where subform = form (n `div` 2) 

Example test

> deftest = verboseCheckResult (\x -> satisfiable x)


> parse' :: Form -> Bool
> parse' x = (parse $ show x) == (parse . show . head) (parse $ show x)

Custom args for our tests if the maxSize exceeds 40 the random data structures might not finish calculation and may result in crashing computers ;)

> customArgs :: Args
> customArgs = stdArgs { maxSize = 40, maxSuccess = 100, chatty = True } 

Test function for the parse functionality can be found below

> parseTest = quickCheckWith customArgs parse'
