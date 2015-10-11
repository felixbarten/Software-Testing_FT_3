> module Exercise2 where


Report
=====
The two functions were benchmarked using the criterion package. The tests can be run by executing the main function of this file. It can even give you pretty charts if you compile it.

Output:

  benchmarking Modular Exponentation/expM 25000234
  time                 3.418 ms   (3.398 ms .. 3.445 ms)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 3.410 ms   (3.403 ms .. 3.421 ms)
  std dev              27.40 μs   (17.21 μs .. 38.62 μs)

  benchmarking Modular Exponentation/expM 35000234
  time                 4.810 ms   (4.764 ms .. 4.859 ms)
                       0.999 R²   (0.999 R² .. 1.000 R²)
  mean                 4.778 ms   (4.761 ms .. 4.805 ms)
  std dev              66.98 μs   (43.74 μs .. 91.76 μs)

  benchmarking Modular Exponentation/exM 25000234
  time                 49.44 μs   (49.02 μs .. 49.84 μs)
                       1.000 R²   (0.999 R² .. 1.000 R²)
  mean                 49.60 μs   (49.32 μs .. 49.96 μs)
  std dev              1.062 μs   (897.3 ns .. 1.222 μs)
  variance introduced by outliers: 18% (moderately inflated)

  benchmarking Modular Exponentation/exM 35000234
  time                 50.23 μs   (49.88 μs .. 50.60 μs)
                       1.000 R²   (0.999 R² .. 1.000 R²)
  mean                 50.32 μs   (50.06 μs .. 50.64 μs)
  std dev              949.6 ns   (754.8 ns .. 1.135 μs)
  variance introduced by outliers: 14% (moderately inflated)


  The results show that the new exM function performs substantially faster than the old expM function.

Ugly code
=========

> import Criterion.Main
> import Exercise1

> main = defaultMain [
>   bgroup "Modular Exponentation" [ bench "expM 25000234"  $  whnf   (expM 4 25000234 ) 497
>                                   ,bench "expM 35000234"  $  whnf   (expM 4 35000234 ) 497
>                                  ],
>   bgroup "Modular Exponentation" [ bench "exM 25000234"  $  whnf   (exM 4 25000234 ) 497
>                                   ,bench "exM 35000234"  $  whnf   (exM 4 35000234 ) 497
>                                  ]
>   ]
