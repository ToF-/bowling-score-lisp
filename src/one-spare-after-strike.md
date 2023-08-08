## One Spare After a Strike

Currently our function for counting extra-points is only recursive in case the rolls do not contain a strike:

```lisp
(defun extra-points (rolls)
  (if (null rolls) 0
    (if (is-strike rolls) (add-strike (cdr rolls))
      (+ (if (is-spare rolls) (caddr rolls) 0)
         (extra-points (cddr rolls))))))
```
Let's add a test showing this.
```lisp
(define-test given-a-spare-after-a-strike-then-extra-points-are-counted
    (assert-equal (+ 10 10 3 4 6 3) (score (list 10 4 6 3))))
(run-tests :all)
```
The result shows that the spare bonus is not being counted.
```
 | Failed Form: (SCORE (LIST 10 4 6 3))
 | Expected 36 but saw 33
 |
GIVEN-A-SPARE-AFTER-A-STRIKE-THEN-EXTRA-POINTS-ARE-COUNTED: 0 assertions passed, 1 failed.
```
We can solve this by adding a recursive call to the function in case of a strike:
```lisp
(defun extra-points (rolls)
  (if (null rolls) 0
    (if (is-strike rolls) 
      (+ (add-strike (cdr rolls))(extra-points (cdr rolls)))
      (+ (if (is-spare rolls) (caddr rolls) 0) 
         (extra-points (cddr rolls))))))
```
All test pass. We can clarify the code a bit by showing the alternatives better.
```lisp
(defun extra-points (rolls)
  (cond ((null rolls) 0)
        ((is-strike rolls) (+ (add-strike (cdr rolls)) (extra-points (cdr rolls))))
        ((is-spare rolls) (+ (caddr rolls) (extra-points (cddr rolls))))
        (t (extra-points (cddr rolls)))))
```
