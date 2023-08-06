## Adding points

```
When only one roll has been thrown, the score is equal to that roll exactly:
```lisp
(load "bowling")

(define-test given-no-roll-score-is-zero
    (assert-equal 0 (score ())))

(define-test given-one-roll-score-is-that-roll
    (assert-equal 7 (score (list 7))))
```
