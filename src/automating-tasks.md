## Automating tasks

To automate our different tasks we can use `make`. With this `Makefile`:
```Makefile
unit: tests.lisp bowling.lisp
	sbcl --script tests.lisp

acceptance: bowling.lisp test-cases.txt expected.txt score.lisp score
	./score <test-cases.txt >results.txt
	diff expected.txt results.txt
```
These tasks cannot be executed as long as the files they depend on are not present. So let's fix that:
```
touch bowling.lisp
touch score.lisp
```
Now we can launch our testing tasks:
```
> make unit
sbcl --script tests.lisp
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
./score <test-cases.txt >results.txt
diff expected.txt results.txt
1,5d0
< 17
< 52
< 300
< 80
< 60
make: *** [acceptance] Error 1
~/Coding/bowling-score/src/lisp:
```
and we are all set to start the kata!
