# Basic Libraries

## 28.6 Exercise: Benchmark Practice

see [src/benchSet.hs](./src/benchSet.hs)

```shell
benchmarking member check map
time                 28.57 ns   (28.10 ns .. 29.11 ns)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 28.43 ns   (28.09 ns .. 28.97 ns)
std dev              1.357 ns   (899.3 ps .. 1.950 ns)
variance introduced by outliers: 70% (severely inflated)

benchmarking member check set
time                 27.93 ns   (27.55 ns .. 28.34 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 27.92 ns   (27.58 ns .. 28.38 ns)
std dev              1.320 ns   (1.010 ns .. 1.940 ns)
variance introduced by outliers: 70% (severely inflated)

benchmarking insert check set
time                 127.9 ns   (127.4 ns .. 128.4 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 127.1 ns   (126.7 ns .. 127.6 ns)
std dev              1.515 ns   (1.162 ns .. 2.043 ns)
variance introduced by outliers: 12% (moderately inflated)

benchmarking insert check set
time                 112.8 ns   (112.4 ns .. 113.3 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 112.6 ns   (112.0 ns .. 113.4 ns)
std dev              2.414 ns   (1.889 ns .. 3.151 ns)
variance introduced by outliers: 30% (moderately inflated)

benchmarking union check set
time                 655.5 μs   (652.0 μs .. 658.8 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 638.7 μs   (634.2 μs .. 642.7 μs)
std dev              15.07 μs   (12.96 μs .. 18.02 μs)
variance introduced by outliers: 14% (moderately inflated)

benchmarking union check set
time                 428.9 μs   (423.5 μs .. 433.8 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 422.6 μs   (419.7 μs .. 425.9 μs)
std dev              10.16 μs   (8.544 μs .. 12.44 μs)
variance introduced by outliers: 15% (moderately inflated)
```

## 28.9 Excercises: Vector

see [src/benchVector.hs](./src/benchVector.hs)

Using unboxed vector has better time performance. I tried measuring space,
but the difference is almost negligable.

## 28.10 Chaper Exercises

### Difference List

see [src/DiffList.hs](./src/DiffList.hs)

It is not actually faster, but it has the same performance as Data.DList and
the same implementation, so I guess this is to be expected?

### A simple queue

see [src/SimpleQueue.hs](./src/SimpleQueue.hs)

Comparing Sequence and Queue seems a bit strange as they serve different
purposes. I could implement a function to add something to the back of the
queue (similar to |>) but that kinda defeats the purpose of queue... I compared
them based on what a queue should be capable of doing and it seem the queue
is better than the sequence in these cases.