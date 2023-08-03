# Benchmarks

Apple M1, 16GB RAM

<!-- $MDX skip -->
```sh
$ ./main.exe
                     name,   major-allocated,    minor-allocated,    monotonic-clock
       rtree/find    1000,          1.115794,        5053.123394,        8706.211211
       rtree/find    3000,          1.617212,        8113.245138,       14296.064470
       rtree/find   10000,         14.722597,       53473.133420,       99040.530336
       rtree/find   50000,        100.259289,      293706.271410,      578093.647431
       rtree/find  100000,         97.256923,      230683.006154,      574174.680000
  rtree/find_load    1000,          0.065361,         590.829372,        1496.124458
  rtree/find_load    3000,          0.032275,         180.012953,        1617.546699
  rtree/find_load   10000,          0.000000,           0.000000,        2112.512840
  rtree/find_load   50000,          0.000000,           0.000000,        2521.168250
  rtree/find_load  100000,          0.000000,           0.000000,        2960.819519
     rtree/insert    1000,      16995.593277,     2027380.880112,     3442481.980392
     rtree/insert    3000,      58014.712281,     6752241.512281,    11343033.196491
     rtree/insert   10000,     254249.145455,    24960718.309091,    44963400.000000
     rtree/insert   50000,    1219452.600000,   141399595.800000,   247627341.600000
     rtree/insert  100000,    2503677.000000,   299104560.000000,   505670500.000000
       rtree/load    1000,      17346.738657,      830784.857641,     1072496.556214
       rtree/load    3000,      86004.129839,     3243578.500806,     4275447.099194
       rtree/load   10000,     420853.892857,    13655703.514286,    18325598.164286
       rtree/load   50000,    3801156.071429,    89015807.928571,   127242285.714286
       rtree/load  100000,    9059167.800000,   200276210.800000,   294776508.600000
```

Hopefully from these benchmarks you can see that `load` is a pretty good optimisation strategy
both in terms of creating a large rtree and in the performance of `find` on that rtree.
