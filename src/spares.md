## Spares on Any Frame

If we redefine rolls _inside the function_ to be starting from the second frame, then we can generalize a bit more:
```lisp
(defun extra-points (rolls)
  (+ (cond ((is-spare rolls) (caddr rolls))
           (t 0))
     (let ((rolls (cddr rolls)))
       (cond ((is-spare rolls) (caddr rolls))
             (t 0)))))
```
We are getting close to recursion. Before getting there, let's add a guard that will guarantee that our function terminates in presence of an empty list. 

Now we can add new test:
```lisp
(define-test given-a-spare-on-any-frame-next-roll-is-added-as-bonus
    (assert-equal (+ 10 3 10 2 10 9 9) (score (list 4 6 3 7 2 8 9))))
```
We respond by adding _yet another_ hard-coded addition:

```lisp
(defun extra-points (rolls)
  (if (null rolls) 0
    (+ (cond ((is-spare rolls) (caddr rolls))
             (t 0))
       (let ((rolls (cddr rolls)))
         (+ (cond ((is-spare rolls) (caddr rolls))
                  (t 0))
            (let ((rolls (cddr rolls)))
              (cond ((is-spare rolls) (caddr rolls))
                    (t 0))))))))
```
And the test passes. Now we can install recursion.
```lisp
(defun extra-points (rolls)
  (if (null rolls) 0
    (+ (cond ((is-spare rolls) (caddr rolls))
             (t 0))
       (extra-points (cddr rolls)))))
```
and simplify a bit more.
```lisp
(defun extra-points (rolls)
  (if (null rolls) 0
    (+ (if (is-spare rolls) (caddr rolls) 0)
       (extra-points (cddr rolls)))))
```
