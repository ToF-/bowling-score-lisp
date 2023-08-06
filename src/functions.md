## Functions

In lisp, calling a function is done by typing a list in which the first element is the function name and all the following elements if any are arguments. Here's an example (yes, `+` is a function):
```lisp
* (+ 42 17)
59
```
A function can be called inside a function call (as long as we don't forget any parenthesis!)
```lisp
* (- (* (+ 42 17) 82) (- (* 4 8) 1))
4807
```
Here are some interesting functions:
```lisp
* (list 1 2 3 4 5)
(1 2 3 4 5)
* (defvar numbers (list 42 17 23 4807))
NUMBERS
* (car numbers)
42
* (cdr numbers)
(17 23 4807)
* (car (cdr numbers))
17
* (cadr numbers)
17
* (caddr numbers)
23
* (length numbers)
4
* (null numbers)
NIL
* (not (null numbers))
T
```
