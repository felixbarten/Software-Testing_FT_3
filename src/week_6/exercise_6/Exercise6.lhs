> module Exercise6 where


> import Lecture6
> import Control.Monad



> testMR k (n:ns) = do
>                result <- primeMR k n
>                print (show result ++ " for number " ++ show n ++ " and K: " ++ show k) 
>                if result 
>                then 
>                   testMR (k+1) ns
>                else 
>                   testMR k ns





> carmichael :: [Integer]
> carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
>       k <- [2..], 
>       isPrime (6*k+1), 
>       isPrime (12*k+1), 
>       isPrime (18*k+1) ]


output: 

*Exercise6> testMillerRabin 1 carmichael 
"True for composite 13079177569 and K: 1"
"True for composite 1042789205881 and K: 1"
"True for composite 1201586232601 and K: 1"
"True for composite 3267961077889 and K: 1"
"True for composite 53269464581929 and K: 1"
"True for composite 57060521336809 and K: 1"
"True for composite 91968282854641 and K: 1"
"True for composite 98445661027561 and K: 1"
"True for composite 129140929242289 and K: 1"
"True for composite 225301895806609 and K: 1"
"True for composite 318705390188641 and K: 1"
"True for composite 351025246957321 and K: 1"
"True for composite 742403294138881 and K: 1"
"True for composite 883519506462529 and K: 1"
"True for composite 938844932257009 and K: 1"
"True for composite 1061085945064681 and K: 1"
"True for composite 1498183378245721 and K: 1"
"True for composite 1687660433615521 and K: 1"
"True for composite 1713289208592601 and K: 1"
"True for composite 2078939720299609 and K: 1"
"True for composite 2408803612382521 and K: 1"
"True for composite 4498600676392369 and K: 1"
"True for composite 4761144691247881 and K: 1"
"True for composite 4925930867128009 and K: 1"
"True for composite 5464294563597481 and K: 1"
"True for composite 5690586528027001 and K: 1"
"True for composite 7943953907064529 and K: 1"
"True for composite 8146186349228281 and K: 1"
"True for composite 9411619439928241 and K: 1"
"True for composite 10699783088092489 and K: 1"
"True for composite 10897931091179161 and K: 1"
"True for composite 11445449444156521 and K: 1"
"True for composite 11563797396433969 and K: 1"
"True for composite 12837231302405329 and K: 1"
"True for composite 14529457281147409 and K: 1"
"True for composite 14674053203612281 and K: 1"
"True for composite 16757353669309849 and K: 1"
"True for composite 22179584528484121 and K: 1"
"True for composite 23673337020655489 and K: 1"
"True for composite 35626003384988881 and K: 1"
"True for composite 39836619437605201 and K: 1"
"True for composite 46325420176593169 and K: 1"
"True for composite 61357874209950529 and K: 1"
"True for composite 66100299894542161 and K: 1"
"True for composite 77203737559865881 and K: 1"
"True for composite 79328213388882361 and K: 1"
"True for composite 81558956130208129 and K: 1"
"True for composite 89747779964545441 and K: 1"
"True for composite 91197951360237361 and K: 1"
.... 

Still a lot of the carmichael numbers pass with k = 1 


*Exercise6> testMillerRabin 10 carmichael 

 Nothing returned after some time 