# Benchmarks

Apple M1, 16GB RAM

<!-- $MDX skip -->
```sh
$ ./main.exe
                     name,   major-allocated,    minor-allocated,    monotonic-clock
       rtree/find    1000,          0.196829,        1510.251383,        3477.069655
       rtree/find    3000,          0.292019,        2461.633463,        5968.352882
       rtree/find   10000,          3.213499,       16037.302631,       54628.218437
       rtree/find   50000,         30.334282,       85764.795164,      701816.897582
       rtree/find  100000,         18.457143,       54300.421429,     1095782.164286
  rtree/find_load    1000,          0.000000,           0.000000,         755.479508
  rtree/find_load    3000,          0.000000,           0.000000,         734.867158
  rtree/find_load   10000,          0.000000,           0.000000,         947.658157
  rtree/find_load   50000,          0.000000,           0.000000,        1173.860563
  rtree/find_load  100000,          0.000000,           0.000000,        1413.278226
     rtree/insert    1000,      16995.593277,     2027380.880112,     3427902.889636
     rtree/insert    3000,      58014.712281,     6752241.512281,    11247354.392982
     rtree/insert   10000,     254249.145455,    24960718.309091,    42015527.236364
     rtree/insert   50000,    1219452.600000,   141399595.800000,   234103050.200000
     rtree/insert  100000,    2503677.000000,   299104560.000000,   491386041.000000
       rtree/load    1000,      17346.738657,      830784.857641,     1072649.987626
       rtree/load    3000,      86004.129839,     3243578.500806,     4281628.390323
       rtree/load   10000,     420853.892857,    13655703.514286,    18343860.735714
       rtree/load   50000,    3801156.071429,    89015807.928571,   127481654.785714
       rtree/load  100000,    9059167.800000,   200276210.800000,   295105141.800000
```

Hopefully from these benchmarks you can see that `load` is a pretty good optimisation strategy
both in terms of creating a large rtree and in the performance of `find` on that rtree.
