# Reading a List of Numbers

Reading several numbers from the input source should be straightforward using recursion. Let's write a new test:
```lisp
; tests.lisp
; …
(define-test given-several-number-read-numbers-give-a-list-with-these-numbers
    (let ((result (with-input-from-string (s "42 17 23") (read-numbers s))))
      (assert-equal (list 42 17 23) result)))
```
To make the test pass, we replace the last `nil` in the function, by a recursive call:
```lisp
(defun read-numbers (source)
  (let ((n (read source NO-EOF-ERROR)))
    (if (null n) nil
      (cons n (read-numbers source)))))
```
Can we try our function of some more numbers, for instance, the numbers that are in the `test-cases.txt` file ? 
```
5
4
3 5 2 7
6
10 5 4 10 5 2
12
10 10 10 10 10 10 10 10 10 10 10 10
10
3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 5
3
10 10 10
```

Let's try!
```lisp
> rlwrap sbcl --load "bowling"
* (defvar s (open "./test-cases.txt"))
S
* (read-numbers s)
(5 4 3 5 2 7 6 10 5 4 10 5 2 12 10 10 10 10 10 10 10 10 10 10 10 10 10 3 5 3 5
 3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 10 10 10)
```
It works!
