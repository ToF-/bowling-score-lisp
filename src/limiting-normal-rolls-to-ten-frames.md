## Limiting Normal Rolls to Ten Frames

First let's make `normal-points` a recursive function again.
```lisp
(defun normal-points (rolls)
  (cond ((null rolls) 0)
        (t (+ (car rolls) (normal-points (cdr rolls))))))
```

Then we incorporate the frame number limit into the addition.
```lisp
(defun normal-points (frame rolls)
  (cond ((null rolls) 0)
        ((>= frame 10) 0)
        ((is-strike rolls) (+ 10 (normal-points (1+ frame) (cdr rolls))))
        ((> (length rolls) 1) (+ (car rolls) (cadr rolls) (normal-points (1+ frame) (cddr rolls))))
        (t (car rolls))))
```
We also have to incorporate the frame number limit into the `extra-points` function in a similar way.
```lisp
(defun extra-points (frame rolls)
  (cond ((null rolls) 0)
        ((>= frame 10) 0)
        ((is-strike rolls) (+ (add-strike (cdr rolls)) (extra-points (1+ frame) (cdr rolls))))
        ((is-spare rolls) (+ (caddr rolls) (extra-points (1+ frame) (cddr rolls))))
        (t (extra-points (1+ frame) (cddr rolls)))))
```
Now we can remove our hard-coded comparison, and start the counting with 0 for the `frame` parameter.
```lisp
(defun score (rolls)
  (+ (normal-points 0 rolls) (extra-points 0 rolls)))
```
The tests pass.  Let's add an assertion about bonus points in the case of spare in tenth frame:
```lisp
(define-test after-tenth-frame-extra-rolls-count-only-as-bonus
    (assert-equal 300 (score (list 10 10 10 10 10 10 10 10 10 10 10 10)))
    (assert-equal 276 (score (list 10 10 10 10 10 10 10 10 10 6 4 10))))
```
The tests still pass. Any other list with extra rolls and no spare or strike in the tenth frame would be an illegal entry.
