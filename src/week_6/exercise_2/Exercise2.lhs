> module Exercise2 where


> import Exercise1
> import Criterion.Main
> import Crypto.Number.ModArithmetic



Dependency on criterion benchmark framework

cabal update
cabal install -j --disable-tests criterion

used version: 0.8.1.0


> main = defaultMain [
>   bgroup "Modular exponentiation" [ bench "expFast 123456789101112"  $ whnf  (expFast 4 123456789101112) 12
>                , bench "expFast 35000234"  $ whnf  (expFast  4 35000234) 497
>                , bench "expSafe 35000234"  $ whnf  (expSafe 4 35000234) 497
>                , bench "exM 35000234" $ whnf  (exM 4 35000234) 497
>                ]
>   ]


warming up
estimating clock resolution...
mean is 1.258616 us (640001 iterations)
found 141312 outliers among 639999 samples (22.1%)
  5 (7.8e-4%) low severe
  141307 (22.1%) high severe
estimating cost of a clock call...
mean is 36.25818 ns (11 iterations)
found 1 outliers among 11 samples (9.1%)
  1 (9.1%) high mild

benchmarking exM/1
mean: 7.493615 us, lb 7.429066 us, ub 7.671183 us, ci 0.950
std dev: 504.0872 ns, lb 213.7383 ns, ub 1.049669 us, ci 0.950
found 10 outliers among 100 samples (10.0%)
  3 (3.0%) high mild
  7 (7.0%) high severe
variance introduced by outliers: 62.586%
variance is severely inflated by outliers

benchmarking exM/5
mean: 4.183355 us, lb 4.173588 us, ub 4.200244 us, ci 0.950
std dev: 64.46170 ns, lb 44.00769 ns, ub 105.7862 ns, ci 0.950
found 7 outliers among 100 samples (7.0%)
  4 (4.0%) high mild
  3 (3.0%) high severe
variance introduced by outliers: 8.467%
variance is slightly inflated by outliers

benchmarking exM/9
mean: 4.224737 us, lb 4.208217 us, ub 4.249284 us, ci 0.950
std dev: 101.5405 ns, lb 75.13697 ns, ub 141.7046 ns, ci 0.950
found 9 outliers among 100 samples (9.0%)
  5 (5.0%) high mild
  4 (4.0%) high severe
variance introduced by outliers: 18.024%
variance is moderately inflated by outliers

benchmarking exM/11
collecting 100 samples, 1 iterations each, in estimated 55.89690 s
mean: 559.5011 ms, lb 558.8700 ms, ub 560.3976 ms, ci 0.950
std dev: 3.822535 ms, lb 2.946266 ms, ub 5.827446 ms, ci 0.950
*Exercise2> 


SSecond run

warming up
estimating clock resolution...
mean is 1.244094 us (640001 iterations)
found 133742 outliers among 639999 samples (20.9%)
  6 (9.4e-4%) low severe
  133736 (20.9%) high severe
estimating cost of a clock call...
mean is 35.60287 ns (11 iterations)
found 1 outliers among 11 samples (9.1%)
  1 (9.1%) high severe

benchmarking Modular exponentiation/expFast 123456789101112
mean: 7.447053 us, lb 7.397012 us, ub 7.653837 us, ci 0.950
std dev: 442.3690 ns, lb 80.79033 ns, ub 1.025371 us, ci 0.950
found 5 outliers among 100 samples (5.0%)
  3 (3.0%) high mild
  2 (2.0%) high severe
variance introduced by outliers: 56.510%
variance is severely inflated by outliers

benchmarking Modular exponentiation/expFast 35000234
mean: 4.196175 us, lb 4.182367 us, ub 4.225182 us, ci 0.950
std dev: 98.10362 ns, lb 54.16347 ns, ub 181.6614 ns, ci 0.950
found 9 outliers among 100 samples (9.0%)
  7 (7.0%) high mild
  2 (2.0%) high severe
variance introduced by outliers: 17.074%
variance is moderately inflated by outliers

benchmarking Modular exponentiation/expSafe 35000234
mean: 4.205070 us, lb 4.193180 us, ub 4.226728 us, ci 0.950
std dev: 80.04900 ns, lb 48.54575 ns, ub 122.0832 ns, ci 0.950
found 4 outliers among 100 samples (4.0%)
  4 (4.0%) high severe
variance introduced by outliers: 12.271%
variance is moderately inflated by outliers

benchmarking Modular exponentiation/exM 35000234
collecting 100 samples, 1 iterations each, in estimated 55.67939 s
mean: 559.1913 ms, lb 558.7374 ms, ub 559.7575 ms, ci 0.950
std dev: 2.579254 ms, lb 2.146316 ms, ub 3.260169 ms, ci 0.950
*Exercise2> 


Even though my implementation could definitely be sped up it will be resolved later. 
Until that time it's functional but the expFast method from the crypto library will be used instead. 
