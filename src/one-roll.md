## One Roll

When only one roll has been thrown, the score is equal to that roll exactly. Let's write a new test.
```lisp
(load "bowling")

(define-test given-no-roll-score-is-zero
    (assert-equal 0 (score ())))

(define-test given-one-roll-score-is-that-roll
    (assert-equal 7 (score (list 7))))
```
Of our two tests, only the first one passes:
```
GIVEN-NO-ROLL-SCORE-IS-ZERO: 1 assertions passed, 0 failed.

 | Failed Form: (SCORE (LIST 7))
 | Expected 7 but saw 0
 |
GIVEN-ONE-ROLL-SCORE-IS-THAT-ROLL: 0 assertions passed, 1 failed.
```
To make the second one pass as well as the first one, we need to introduce a condition in our function: if the list of roll is empty, return 0, in any other case, return the first roll in the list.
```lisp
(defun score (rolls)
  (cond ((null rolls) 0)
        (t (car rolls))))
```
And now all the tests pass.
