# Benchmarks

Apple M1, 16GB RAM

<!-- $MDX skip -->
```sh
$ ./main.exe
                     name,   major-allocated,    minor-allocated,    monotonic-clock
       rtree/find    1000,         22.482602,       11434.014178,       36161.414361
       rtree/find    3000,        599.462841,       61198.511145,      154306.330679
       rtree/find   10000,        562.178754,       71162.894893,      238068.722506
       rtree/find   50000,      51598.549231,      669875.089231,     2507542.670769
       rtree/find  100000,     607076.218182,     3336361.745455,    14895394.781818
  rtree/find_load    1000,         34.840884,       10815.383686,       29690.071598
  rtree/find_load    3000,         22.763925,       12369.517640,       37207.521450
  rtree/find_load   10000,       5765.172086,      196573.338619,      508915.990793
  rtree/find_load   50000,      14895.625668,      262142.690508,      804282.209225
  rtree/find_load  100000,     361933.483516,     2517719.021978,     8899765.175824
     rtree/insert    1000,      24868.717647,     1435836.273389,     3451747.033053
     rtree/insert    3000,      83835.445614,     5081884.470175,    12129753.185965
     rtree/insert   10000,     295946.818182,    18483426.309091,    44615157.581818
     rtree/insert   50000,    1564458.800000,   104699701.400000,   253329992.000000
     rtree/insert  100000,    3177467.000000,   218102426.000000,   531844084.000000
       rtree/load    1000,      24613.019931,      759049.107429,      748570.480378
       rtree/load    3000,     117296.469417,     2973895.957800,     3042270.611190
       rtree/load   10000,     539572.564912,    12582669.782456,    13139363.912281
       rtree/load   50000,    4503152.714286,    82723680.642857,    92038931.357143
       rtree/load  100000,   11117734.200000,   186223932.000000,   221476758.200000
```

Hopefully from these benchmarks you can see that `load` is a pretty good optimisation strategy
both in terms of creating a large rtree and in the performance of `find` on that rtree.
