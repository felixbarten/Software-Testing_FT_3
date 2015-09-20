> module Exercise2 where

> import Test.QuickCheck
> import Control.Monad
> import Lecture3

Report: Exercise 2
==========

The parse function is tested using quickCheck. Arbitrary Forms are generated and they are used to test the following properties:

 - That the parser can parse valid forms
 - That the parser is idempotent

*Time Spent: 6 hours (long enough)*

> instance Arbitrary Form where
>    arbitrary = form

> form = sized form'
> form' 0 = liftM Prop (choose(1,10) :: Gen Int)
> form' n | n > 0 = oneof [
>                     liftM Neg subform,
>                     liftM Cnj (listOf subform),
>                     liftM Dsj (listOf subform),
>                     liftM2 Impl subform subform,
>                     liftM2 Equiv subform subform
>                     ]
>               where subform = form' (n `div` 2)

> formToString :: Form -> String
> formToString x = show x

> prop_idempotentParser :: Form -> Bool
> prop_idempotentParser x = (parse.formToString) x == (parse . formToString . head  . parse . formToString) x

> prop_canParseValidForm :: Form -> Bool
> prop_canParseValidForm x = (parse .formToString)  x /= [] 

> testParse = verboseCheckResult prop_idempotentParser
> testParse' = verboseCheckWith stdArgs {maxSize = 10}  prop_idempotentParser
> testParse'' = verboseCheckWith stdArgs {maxSize = 10}  prop_canParseValidForm
