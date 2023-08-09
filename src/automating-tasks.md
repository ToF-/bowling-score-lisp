# Automating tasks
To automate our different tasks we can use `make`. With this `Makefile`:
```Makefile
unit: tests.lisp bowling.lisp
	sbcl --script tests.lisp

accept: bowling.lisp test-cases.txt expected.txt score.lisp
	sbcl --script score.lisp <test-cases.txt >results.txt
	diff expected.txt results.txt
```
These tasks cannot be executed as long as the files they depend on are not present. So let's fix that:
```
> touch bowling.lisp
> echo "(print 42) (terpri)" >score.lisp
```
Now we can launch our testing tasks:
```
> make unit
sbcl --script tests.lisp
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

> make accept
sbcl --script score.lisp <test-cases.txt >results.txt
diff expected.txt results.txt
1,5c1,2
< 17
< 52
< 300
< 80
< 60
---
> 
> 42 
make: *** [Makefile:6 : accept] Error 1
```

