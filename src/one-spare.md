## One Spare on the First Frame

If the first two rolls knocked down all the pins, the next roll will be added as a bonus.
```lisp
(define-test given-a-spare-on-first-rolls-third-roll-is-added-as-bonus
    (assert-equal 18 (score (list 3 7 4))))
```
As expected the test fails.
```
 | Failed Form: (SCORE (LIST 3 7 4))
 | Expected 18 but saw 14
 |
GIVEN-A-SPARE-ON-FIRST-ROLLS-THIRD-ROLL-IS-ADDED-AS-BONUS: 0 assertions passed, 1 failed.
```
Since there is no obvious implementation to make it pass, let's fake it with a hard-coded condition.
```lisp
(defun score (rolls)
  (cond ((equal (list 3 7 4) rolls) 18)
        (t (apply '+ rolls))))
```
We should also extract functions to have better separation of concerns: on one side, the normal score made with actual throws, on the other side, the extra points.
```lisp
(defun extra-points (rolls)
  (cond ((equal (list 3 7 4) rolls) 4)
         (t 0)))

(defun normal-points (rolls)
  (apply '+ rolls))

(defun score (rolls)
  (+ (normal-points rolls) (extra-points rolls)))
```
We still have a _fake it_ implementation. Let's add an assertion to the test.
```lisp
(define-test given-a-spare-on-first-rolls-third-roll-is-added-as-bonus
    (assert-equal 18 (score (list 3 7 4)))
    (assert-equal 28 (score (list 2 8 9))))
```
Firt we make it pass:
```lisp
(defun extra-points (rolls)
  (cond ((equal (list 3 7 4) rolls) 4)
        ((equal (list 2 8 9) rolls) 9)
        (t 0)))
```
Then we extract the study of the rolls in a function.
```lisp
(defun is-spare (rolls)
  (or (equal (list 3 7 4) rolls)
      (equal (list 2 8 9) rolls)))

(defun extra-points (rolls)
  (cond ((is-spare rolls) (caddr rolls))
        (t 0)))
```
And then we generalize the function.
```lisp
(defun is-spare (rolls)
  (cond ((< (length rolls) 3) nil)
        (t (= 10 (+ (car rolls) (cadr rolls))))))
```
