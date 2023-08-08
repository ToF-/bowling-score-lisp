## One Spare on the Second Frame
Of course a spare can happen, not on the first frame, but on the second, for example. Let's write a new test.
```lisp
(define-test given-a-spare-on-second-frame-next-roll-is-added-as-bonus
    (assert-equal 22 (score (list 4 0 3 7 4))))
```
Let's respond to this falling test with a _fake_:
```lisp
(defun extra-points (rolls)
  (cond ((equal (list 4 0 3 7 4) rolls) 4)
        ((is-spare rolls) (caddr rolls))
        (t 0)))
```
Now we want to make `extra-points` a recursive function. But first we have to separate concerns: the hard-coded extra points are _added_ to the extra points (if any) for the first frame:
```lisp
(defun extra-points (rolls)
  (+ (cond ((is-spare rolls) (caddr rolls))
           (t 0))
     (cond ((equal (list 4 0 3 7 4) rolls) 4)
           (t 0))))
```
