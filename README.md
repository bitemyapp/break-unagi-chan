# break-unagi-chan

Experiment to see if I could reproduce loss from mishandling unagi-chan and `race`. I did.

```
stack exec -- break-unagi-chan-exe
delayed!
delayed!
delayed!
delayed!
delayed!
delayed!
delayed!
delayed!
delayed!
9991
SKIP DETECTED: 6194 6196
SKIP DETECTED: 6847 6849
SKIP DETECTED: 7564 7566
SKIP DETECTED: 7806 7809
SKIP DETECTED: 8098 8100
SKIP DETECTED: 8797 8799
SKIP DETECTED: 8816 8818
SKIP DETECTED: 8818 8820
10000
````

## Timeout seems to work better

Rather than weird periodic loss, `timeout` seems to skip/drop values less often than `race`'ing against a threadDelay:

```
$ make run
stack build
stack exec -- break-unagi-chan-exe
10000
10000
0
Nothing
[ callen@chalcis ~/work/break-unagi-chan master ✗ ]
$ make run
stack build
stack exec -- break-unagi-chan-exe
delayed on 2687.0
9999
9999
1
Just 10000
[ callen@chalcis ~/work/break-unagi-chan master ✗ ]
$ make run
stack build
stack exec -- break-unagi-chan-exe
delayed on 5134.0
9999
SKIP DETECTED: 5133 5135
10000
0
Nothing
```

Much of the time, I got this benign result:

```
10000
10000
0
Nothing
```
