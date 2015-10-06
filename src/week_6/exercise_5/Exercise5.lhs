> module Exercise5 where


> import Lecture6
> import Control.Monad


> testFermat k xs= filterM (\c -> tstFer k c) xs

 
> tstFer k n = do
>                result <- prime_tests_F k n
>                when (result) $ print ("True for composite " ++ show n ++ " and K: " ++ show k) 
>                return result


> carmichael :: [Integer]
> carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
>       k <- [2..], 
>       isPrime (6*k+1), 
>       isPrime (12*k+1), 
>       isPrime (18*k+1) ]


Copied code from exercise 3 below


> composites :: [Integer]
> composites = filter (not.isPrime) [3..]

Output: 


With K = 10
*Exercise5> testFermat 10 carmichael 
"True for composite 181913824914115321 and K: 10"
"True for composite 182439383530479121 and K: 10"
"True for composite 185294899452290041 and K: 10"
"True for composite 187268560492386601 and K: 10"
"True for composite 187707855202300369 and K: 10"
"True for composite 187976054697378769 and K: 10"
"True for composite 189417959386273801 and K: 10"
"True for composite 190347234557112289 and K: 10"
"True for composite 190932283428508441 and K: 10"
"True for composite 191801224158108121 and K: 10"
"True for composite 191910026758200481 and K: 10"
"True for composite 192454656999290281 and K: 10"
"True for composite 194984228428868089 and K: 10"
"True for composite 196472760620348449 and K: 10"
"True for composite 198469228131307729 and K: 10"
"True for composite 198803281969136809 and K: 10"
"True for composite 201714130626423769 and K: 10"
"True for composite 203406319720749169 and K: 10"
"True for composite 203689268065862569 and K: 10"
"True for composite 204131191240756081 and K: 10"
"True for composite 204199236102357289 and K: 10"
"True for composite 204482919124364689 and K: 10"
"True for composite 205950899890445041 and K: 10"
"True for composite 206750411108827561 and K: 10"
"True for composite 207724024576393921 and K: 10"
"True for composite 209564991465871681 and K: 10"
"True for composite 210385392668769529 and K: 10"
"True for composite 214742306940454801 and K: 10"
"True for composite 214812689764715209 and K: 10"
.....

*Exercise5> 

With K = 100 still a lot of the carmichael numbers are passing.

When changed to 3000 a lot less 

*Exercise5> testFermat 3000 carmichael 
"True for composite 727993807201 and K: 3000"
"True for composite 1201586232601 and K: 3000"
"True for composite 1920595706641 and K: 3000"
"True for composite 2028691238689 and K: 3000"
"True for composite 2920883888089 and K: 3000"
"True for composite 3091175755489 and K: 3000"
"True for composite 3267961077889 and K: 3000"
"True for composite 3296857440241 and K: 3000"
"True for composite 3414146271409 and K: 3000"
"True for composite 3711619793521 and K: 3000"
"True for composite 3719466204049 and K: 3000"
"True for composite 3878725359169 and K: 3000"
"True for composite 4287981117241 and K: 3000"
"True for composite 4507445537641 and K: 3000"
"True for composite 7622722964881 and K: 3000"
...
