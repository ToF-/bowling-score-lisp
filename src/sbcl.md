# SBCL
[SBCL](https://www.sbcl.org/) is the most popular implementation of common lisp, and it generates machine code that is fast (not that it matters much for our kata!).

To quote [Wikipedia](https://en.wikipedia.org/wiki/Steel_Bank_Common_Lisp):

> Steel Bank Common Lisp (SBCL) is a free Common Lisp implementation that features a high-performance native compiler, Unicode support and threading.
>
> The name "Steel Bank Common Lisp" is a reference to Carnegie Mellon University Common Lisp from which SBCL forked: Andrew Carnegie made his fortune in the steel industry and Andrew Mellon was a successful banker.

## installing sbcl

On a Mac with macos:
```
> brew install sbcl
```

On a linux machine with ubuntu:
```
> sudo apt install sbcl
```

To check your installation:
```
> sbcl --version
SBCL 2.3.4
```

## wrapping interactive sessions with `rlwrap`

As `sbcl` doesn't allow command editing and navigation in command history, we will wrap our sbcl sessions with the command: `rlwrap sbcl`.

On a Mac with macos:
```
> brew install rlwrap
```

On a linux machine with ubuntu:
```
> sudo apt install rlwrap
```
## the Read-Eval-Print Loop
Let's play a bit with *sbcl*.
```
> rlwrap sbcl
This is SBCL 2.3.4, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
*
```
Sbcl is waiting for an input from the user. To leave the REPL, enter `(exit)` or type `<CTL-D>`
```
* 42
42
* 17
17
* (exit)
>
```
## Functions

In lisp, calling a function is done by typing a list in which the first element is the function name and all the following elements if any are arguments. Here's an example (yes, `+` is a function):
```
* (+ 42 17)
59
```
A function can be called inside a function call (as long as we don't forget any parenthesis!)
```
* (- (* (+ 42 17) 82) (- (* 4 8) 1))
4807
```
Here are some interesting functions:
```
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
## Quote
Not every value in lisp is a number. To represent symbolic values, we can use the `quote` function, or  `'`:
```
* (quote foo)
FOO
* 'bar
BAR
```
We can quote symbols but also lists (of symbols or whatever). `quote` means: _don't evaluate this_.
```
* (quote (foo bar qux))
(FOO BAR QUX)
* '(+ 17 23)
(+ 17 23)
```
## Conditions
`T` and `NIL` are the boolean values for respectively *true* and *false*. `NIL` is also the value for empty lists. To evaluate expressions according to a condition, we can use the `if` form:
```
* (if (= 42 (* 6 7)) 'correct 'wrong)
CORRECT
```
or we can use the `cond` form:
```
* (cond ((= 41 (* 6 7)) 'correct)
        ((= 23 (* 6 4)) 'wrong)
        (t '(everything above was false)))
(EVERYTHING ABOVE WAS FALSE)
```
## Functions
Functions can be defined with `defun`, followed by the name of the function, and the parameters (if any) between parentheses, then the body of the function.
```
* (defun times-two (x) 
    (* x 2))
TIMES-TWO
* (times-two 42)
84
```
## Variable bindings
It is possible, using `let` to define *variables* and bind them to some values that are used in the last expression of the `let` form.
```
* (let ((x 17)
        (y 23))
     (+ x y))
40
```
The `let*` form allows for defining bindings with variables previously bound in the same list. (`let` won't let you do that).
```
* (let ((x 17)
        (y (+ 23 x))
    (+ x y))
; in: LET ((X 17) (Y (+ 23 X)))
;     (+ 23 X)
;
; caught WARNING:
;   undefined variable: COMMON-LISP-USER::X
;
; compilation unit finished
;   Undefined variable:
;     X
;   caught 1 WARNING condition

debugger invoked on a UNBOUND-VARIABLE @535733C0 in thread
#<THREAD "main thread" RUNNING {10044A0113}>:
  The variable X is unbound.
…
(let* ((x 17)
       (y (+ x 23)))
   (+ x y))
57
```

## Lists and recursion
Recursive functions — functions that call themselves — work very well with lists. Here's an example:
```(
* (defun size (l)
    (if (null l)
        0 
        (1+ (size (cdr l)))
    )
  )
SIZE
* (size '(A B C D E))
5
```
## Creating Lists
How do we construct a list element by element? We use `cons`:
```
* (cons 'A nil)
(A)
* (cons 'A (cons 'B (cons 'C nil)))
(A B C)
```
Here's a function that creates the list of all numbers between *n* and *m*:
```
* (defun seq (n m)
    (if (> n m)
        nil
        (cons n (seq (1+ n) m))
    )
  )
SEQ
* (seq 17 23)
(17 18 19 20 21 22 23)
```
## Executing lisp scripts
To execute a script written in lisp with sbcl, launch sbcl with the `--script` option. Suppose we have a script file name `my-script.lisp` that prints the result of an addition,
```
; my-script.lisp  prints the result of an addition
(print (+ 23 17 2))
```
then this script can be executed as a stand-alone program:
```
> sbcl --script my-script.lisp
42
```

