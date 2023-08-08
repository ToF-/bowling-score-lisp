## One Strike With No Other Rolls

As it is, calculating strike bonus this way poses a problem.
```lisp
(defun is-strike (rolls)
  (cond ((< (length rolls) 1) nil)
        (t (= 10 (car rolls)))))

(defun extra-points (rolls)
  (if (null rolls) 0
    (if (is-strike rolls) (+ (cadr rolls) (caddr rolls))
      (+ (if (is-spare rolls) (caddr rolls) 0)
         (extra-points (cddr rolls))))))
```
What if there's a strike not followed by any roll yet ? Let's add a test.
```lisp
(define-test given-a-strike-on-first-roll-with-no-other-roll-then-no-bonus-yet
    (assert-equal 10 (score (list 10))))
```
And our code is causing an execution error. Let's try to debug it:
```
> sbcl --load bowling.lisp
* (score (list 10))

debugger invoked on a TYPE-ERROR @52A03A84 in thread
#<THREAD "main thread" RUNNING {10044A8113}>:
  The value
    NIL
  is not of type
    NUMBER
```
We can solve this by coding our own addition for strike points:
```lisp
(defun add-strike (rolls)
  (cond ((null rolls) 0)
        ((< (length rolls) 2) (car rolls))
        (t (+ (car rolls) (cadr rolls)))))

(defun extra-points (rolls)
  (if (null rolls) 0
    (if (is-strike rolls) (add-strike (cdr rolls))
      (+ (if (is-spare rolls) (caddr rolls) 0)
         (extra-points (cddr rolls))))))
```
