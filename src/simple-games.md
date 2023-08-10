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
Here's another case, one roll.
```
(define-test when-only-one-rolls-score-is-this-roll
    (assert-equal 7 (score (list 7))))
```
To make it pass, we add a `cond` on the rolls list:
```
(defun score (rolls)
  (cond ((null rolls) 0)
        (t (car rolls))))
```
From here we can quickly progress. Let's add the case for several rolls, no bonus throw.
```
(define-test when-given-average-rolls-score-is-the-sum-of-the-rolls
    (assert-equal 27 (score (list 4 5 3 6 2 7))))
```
We make it pass by installing recursion:
```
(defun score (rolls)
  (cond ((null rolls) 0)
        (t (+ (car rolls) (score (cdr rolls))))))
```
