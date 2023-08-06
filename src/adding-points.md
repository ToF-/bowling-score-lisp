## Adding points

Let's add a new case: two rolls have been thrown, so the score is now equal to the sum of these two rolls.
```lisp
(define-test given-two-rolls-score-is-their-sur
    (assert-equal 8 (score (list 2 6))))
```
As expected, the third test fails:
```
 | Failed Form: (SCORE (LIST 2 6))
 | Expected 8 but saw 2
 |
GIVEN-TWO-ROLLS-SCORE-IS-THEIR-SUR: 0 assertions passed, 1 failed.
```

To make it pass, we can start by adding a condition on the length of the list:
```lisp
(defun score (rolls)
  (cond ((null rolls) 0)
        ((= 2 (length rolls)) (+ (car rolls) (cadr rolls)))
        (t (car rolls))))
```
All tests pass. We can refactor the function, using a recursive call, since the function already detects an empty list.
```lisp
(defun score (rolls)
  (cond ((null rolls) 0)
        (t (+ (car rolls) (score (cdr rolls))))))
```
