## Reading a List of Numbers

In order to read a list of numbers, we need to
- read a number, which we already know how to do
- build a list by adding an element to a (previously formed or empty) list
- recursively call a function (which we also know how to do) 

The function `cons` is used to build a list. Here are examples:
```lisp
> rlwrap sbcl
* (cons 42 nil)
(42)
*(cons 42 (cons 17 (cons 4807 nil)))
(42 17 4807)
```
The following script will read a list of values on the standard input and them print that list:
```lisp
; read-list.lisp
(defun read-list ()
  (let ((n (read *standard-input* nil)))
    (if (null n) nil (cons n (read-list)))))

(format t "~a~%" (read-list))
```
We can use it directly by typing numbers:
```
> sbcl --script read-list.lisp
42 23 17 ↵
4807 ↵
<ctl-d>
(42 23 17 4807)
```
Or use it with any file, using file redirection:
```
> sbcl --script read-list.lisp <test-cases.txt
(5 4 3 5 2 7 6 10 5 4 10 5 2 12 10 10 10 10 10 10 10 10 10 10 10 10 10 3 5 3 5
 3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 10 10 10)
```
Now that we know how to read the standard input stream for number, we can resume our exploration of the bowling score problem.
