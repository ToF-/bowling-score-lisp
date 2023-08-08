## One Strike on the First Frame

Let's continue with bonus points. Here's a test for a strike on the first frame.

```lisp
(define-test given-a-strike-on-first-roll-roll-2-and-3-are-added-as-bonus
    (assert-equal 20 (score (list 10 3 2))))
```
The test fails:
```
 | Failed Form: (SCORE (LIST 10 3 2))
 | Expected 20 but saw 15
 |
GIVEN-A-STRIKE-ON-FIRST-ROLL-ROLL-2-AND-3-ARE-ADDED-AS-BONUS: 0 assertions passed, 1 failed.
```
First, _fake it_:
```lisp
(defun extra-points (rolls)
  (if (null rolls) 0
    (if (equal (list 10 3 2) rolls) 5
      (+ (if (is-spare rolls) (caddr rolls) 0)
         (extra-points (cddr rolls))))))
```
Then extract a function and generalize a bit.
```lisp
(defun is-strike (rolls)
  (equal (list 10 3 2) rolls))

(defun extra-points (rolls)
  (if (null rolls) 0
    (if (is-strike rolls) (+ (cadr rolls) (caddr rolls))
      (+ (if (is-spare rolls) (caddr rolls) 0)
         (extra-points (cddr rolls))))))
```
Let's triangulate with another assertion:
```lisp
(define-test given-a-strike-on-first-roll-roll-2-and-3-are-added-as-bonus
    (assert-equal 20 (score (list 10 3 2)))
    (assert-equal 28 (score (list 10 8 1))))
```
We can generalize the `is-strike` function:
```lisp
(defun is-strike (rolls)
  (cond ((< (length rolls) 1) nil)
        (t (= 10 (car rolls)))))
```
