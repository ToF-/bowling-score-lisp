# Setting up our test tools

## Unit tests
In a file named `tests.lisp`, let start with a failing test
```lisp
(ql:quickload :lisp-unit)
(in-package :lisp-unit)
(setq *print-failures* t)

(define-test dummy-test
    (assert-equal 5 (+ 2 2)))

(run-tests :all)
(sb-ext:quit)
```

running `sbcl --noinform --load tests.lisp` yields the following output:
```lisp
; Loading "lisp-unit"

 | Failed Form: (+ 2 2)
 | Expected 5 but saw 4
 |
DUMMY-TEST: 0 assertions passed, 1 failed.

Unit Test Summary
 | 1 assertions total
 | 0 passed
 | 1 failed
 | 0 execution errors
 | 0 missing tests
DUMMY-TEST: 1 assertions passed, 0 failed.
```

Let's change the test:
```lisp
(define-test dummy-test
    (assert-equal 5 (+ 3 2)))
```
And see that the test passes:
```
DUMMY-TEST: 1 assertions passed, 0 failed.

Unit Test Summary
 | 1 assertions total
 | 1 passed
 | 0 failed
 | 0 execution errors
 | 0 missing tests
 ```

## Acceptance tests

Our acceptance test will be based on this `sample.txt` file:
```
3 5 2 7
10 5 4 10 5 2
10 10 10 10 10 10 10 10 10 10 10 10
3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 5
10 10 10
```
and the following `expected.txt` file:
```
17
52
300
80
60
```

A little lisp program `score.lisp` will compute the score of each game included in the given file. For now, this program only prints a usage message.
```lisp
(format t "usage : sbcl --noinform --load score.lisp --games <GAMEFILE.TXT>~%")
(sb-ext:quit)
```

Executing the program can be simplified through a script:
```bash
#!/bin/bash
sbcl --noinform --load score.lisp --games $1
```
This script should be made executable with:
```
> chmod +x score
```
## Automating tasks

We create a Makefile containing these two recipes:
```makefile
unit: tests.lisp bowling.lisp
	sbcl --noinform --load tests.lisp

acceptance: bowling.lisp sample.txt
	score sample.txt >result.txt
	diff expected.txt result.txt
```

## What we have so far

Running our tests, we see a passing (dummy) unit tests, and an acceptance test failure:
```
> make unit
sbcl --noinform --load tests.lisp
To load "lisp-unit":
  Load 1 ASDF system:
    lisp-unit
; Loading "lisp-unit"

DUMMY-TEST: 1 assertions passed, 0 failed.

Unit Test Summary
 | 1 assertions total
 | 1 passed
 | 0 failed
 | 0 execution errors
 | 0 missing tests

> make acceptance
./score sample.txt >result.txt
diff expected.txt result.txt
1,5c1
< 17
< 52
< 300
< 80
< 60
---
> usage : sbcl --noinform --load score.lisp --games <GAMEFILE.TXT>
make: *** [acceptance] Error 1
```

