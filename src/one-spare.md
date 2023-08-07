## One Spare

If the first two rolls knocked down all the pins, the next roll will be added as a bonus.
```lisp
(define-test given-a-spare-on-first-rolls-third-roll-is-added-for-bonus
    (assert-equal 18 (score (list 3 7 4))))
```
