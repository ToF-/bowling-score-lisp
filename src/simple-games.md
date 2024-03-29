# Simple Games

Starting with the simplest case allows us to take care of the inevitable mistakes in writing code in a language that we don't master.
Here's a simple case: when no roll has been thrown yet, the score is zero.
```
; tests.lisp
(require "lisp-unit" "~/.local/share/common-lisp/source/lisp-unit.lisp")
; set up testing options
(in-package :lisp-unit)
(setq *print-failures* t)
(load "bowling")

(define-test when-no-rolls-yet-score-is-zero
    (assert-equal 0 (score ())))

(run-tests :all)
```
We make the test pass with the simplest code that can possibly work:
```
; bowling.lisp
(defpackage :bowling
(:export :score))

(defun score (rolls)
    0)
```
That was easy. Here's another case, one roll:
```
(define-test when-only-one-rolls-score-is-this-roll
    (assert-equal 7 (score (list 7))))
```
To make it pass, we add a `cond` on the rolls list: if the list is empty, the score is zero, otherwise, return the first roll value from the list:
```
(defun score (rolls)
  (cond ((null rolls) 0)
        (t (car rolls))))
```

Let's add a simple test case, two rolls:


From here we can make fast progress. Let's add a test for several rolls, with still no spare or strike bonuses to have to account for:
```
(define-test when-given-average-rolls-score-is-the-sum-of-the-rolls
    (assert-equal 27 (score (list 4 5 3 6 2 7))))
```
We make this new test pass by installing recursion in the `score` function:
```
(defun score (rolls)
  (cond ((null rolls) 0)
        (t (+ (car rolls)
              (score (cdr rolls))))))
```

So far, we have a simple calculation scheme : recursively add the numbers in the list. The frame number and the position of a new frame in the list don't matter here; we didn't care to write any code to detect these, since we don't have tests for spare, strike or tenth frame yet. 

```
(define-test when-two-rolls-score-is-the-sum-of-rolls
    (assert-equal 9 (score (list 4 5))))
```

To make this test pass, we change the `cond` to respond to 3 different situations : null length, length of 1, and otherwise length of 2.

```
(defun score (rolls)
  (cond ((null rolls) 0)
        ((eql 1 (length rolls)) (car rolls))
        (t (+ (car rolls) (cadr rolls)))))
```

What this code does is obvious though, whatever the situation is : sum the rolls in the list, as the following sequence of sbcl shows:
```
(+)
0
(+ 7)
7
(+ 4 5)
9
```
The `+` function is really a *sum* function: it can be applied to any list list, even the empty list.

To apply a function to a list, we use the `apply` function:
```
(apply #'+ ())
0
(apply #'+ (list 7))
7
(apply #'+ (list 4 5))
9
```
Thus, our current version of `score` can be simplified:
```
(defun score (rolls)
  (apply #'+ rolls))
```

and it will correctly compute the score for any input list of 0, 1 or 2 rolls.



