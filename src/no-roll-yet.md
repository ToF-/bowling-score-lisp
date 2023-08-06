## No Roll Yet
Let's start with a very simple case: when no roll has been thrown yet, the score of a game is zero. 
```lisp
(load "bowling")

(define-test given-no-roll-score-is-zero
    (assert-equal 0 (score ())))

(run-tests :all)
```
Running the test with `make unit` we get this message:
```
GIVEN-NO-ROLL-SCORE-IS-ZERO: 0 assertions passed, 0 failed, and an execution error.

Unit Test Summary
 | 0 assertions total
 | 0 passed
 | 0 failed
 | 1 execution errors
 | 0 missing tests
```
since the `bowling.lisp` script is empty.

To make this test pass, we  have to fill it with a package `bowling` which will exports a function `score`. Having this function return 0 is enough to make the test pass, so we do that.
```lisp
(defpackage :bowling
(:export :score))

(defun score (rolls)
    0)
```
And now our first test passes:
```
> make unit
sbcl --script tests.lisp

GIVEN-NO-ROLL-SCORE-IS-ZERO: 1 assertions passed, 0 failed.

Unit Test Summary
 | 1 assertions total
 | 1 passed
 | 0 failed
 | 0 execution errors
 | 0 missing tests
```
Hurray !
