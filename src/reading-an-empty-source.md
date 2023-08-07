# Reading Numbers on an Input Source

Let's write a fist test: when the source is empty, then `nil` is returned.
```lisp
; tests.lisp
; …
(define-test given-no-numbers-read-numbers-give-nil
    (let ((result (with-input-from-string (s "") (read-numbers s))))
      (assert-equal nil result)))
```
We get an execution error, since the `read-numbers` function is not defined.
Let' do the simplest thing that can possibly work: fake it.

```lisp
(defpackage :bowling
(:export :score :read-numbers))

(defun score (rolls)
  (cond ((null rolls) 0)
        (t (+ (car rolls) (score (cdr rolls))))))

(defun read-numbers (source)
  nil)
```
And now the test pass.


