# Input & Output
## Reading from standard input stream

The `read` function can read any value from the standard input. Here's an example: the following expression waits for a number on the standard input and then calculate its square.
```
* (let ((x (read))) (* x x))
42 ↵
1764
```
## Reading From a File
If we can read values from the standard input as a source, it must be possible to read from another source ?

Of course! Let's try it:
```
> echo "4807" >value.txt
> rlwrap sbcl
* (defvar *my-stream* (open "./value.txt"))
MY-STREAM
* (read *my-stream*)
4807
```

Can we read _several_ values, and put them in a list ? Again, let's try.
```
> echo "42" >values.txt
> echo "17" >>values.txt
> echo "23" >>values.txt
> cat values.txt
42
17
23
> rlwrap sbcl
* (defvar *s* (open "./values.txt"))
*S*
* (let ((v1 (read *s*)) (v2 (read *s*)) (v3 (read *s*)))
    (cons v1 (cons v2 (cons v3 nil))))
(42 17 23)
```
## Reading from a string
The `read` function is truly powerful. It can actually read from any _stream_, be it the standard input stream, an opened file, or a stream formed with a given string.

The function `with-input-from-string` allow us to 
- define a named stream
- use it in a form as a source stream for `read`

Here are examples:
```
* (with-input-from-string (s "42") (read s))
42
* (with-input-from-string (s  "42 17") (cons (read s) (cons (read s) NIL)))
(42 17)
```
## Reading numbers from a file source
Let's build the function we need, one step at a time.
### Reading an empty source
Starting with trivial cases, when given a empty source, the function should return `NIL`.
```
(define-test given-no-numbers-read-numbers-give-nil
    (let ((result (with-input-from-string (s "") (read-numbers s))))
      (assert-equal nil result)))
```
Making this test pass with a _fake_:
```
; bowling.lisp
(defpackage :bowling
(:export :score :read-numbers))

(defun read-numbers (source)
  nil)
```
Next, we add a test for reading a single value.
```
(define-test given-one-number-read-numbers-give-a-list-with-this-number
    (let ((result (with-input-from-string (s "42") (read-numbers s))))
      (assert-equal (list 42) result)))
```
To make our second test pass, reading one number, we have to detect that a numbers has been read from the source, or not. If there was a number, return a list with that number inside, if not, return `nil`.
```lisp
(defun read-numbers (source)
  (let ((n (read source)))
    (if (null n) nil
      (cons n nil))))
```
but now the result is unexpected: our test for empty source has an execution error:
```
GIVEN-NO-NUMBERS-READ-NUMBERS-GIVE-NIL: 0 assertions passed, 0 failed, and an execution error.

GIVEN-ONE-NUMBER-READ-NUMBERS-GIVE-A-LIST-WITH-THIS-NUMBER: 1 assertions passed, 0 failed.
``` 
We have to try the function interactively to understand what is happening:
```
sbcl --load bowling.lisp
* (with-input-from-string (s "42") (read-numbers s))
(42)
* (with-input-from-string (s "") (read-numbers s))

debugger invoked on a END-OF-FILE in thread
#<THREAD "main thread" RUNNING {10044A0113}>:
  end of file on #<dynamic-extent STRING-INPUT-STREAM (unavailable) from "">
```
## Dealing with EOF

The "end of file" situation on our stream is an error condition that interrupts our program. 
This can be fixed with one of the (optional) parameters of `read`.

> Function **READ**\
> - Syntax:\
>   - **read** *&optional input-stream eof-error-p eof-value recursive-p* => *object*\
> - Arguments and Values:\
>   - *input-stream* — an input stream designator.\
>   - *eof-error-p* — a generalized boolean. The default is true.\
>   - *eof-value* — an object. The default is nil.\
>   - *recursive-p* — a generalized boolean. The default is false.\
>   - *object* — an object (parsed by the Lisp reader) or the eof-value.\
> …
>
> If a file ends in a symbol or a number immediately followed by an end of file, **read** reads the symbol or number successfully; when called again, it sees the end of file and only then acts according to *eof-error-p*.

We have to indicate that the end of file shouldn't be an error. Let's use a constant to document this.
```
(defconstant NO-EOF-ERROR nil)

(defun read-numbers (source)
  (let ((n (read source NO-EOF-ERROR)))
    (if (null n) nil
      (cons n nil))))
```
Now all the tests pass.
# Reading a List of Numbers

Reading several numbers from the input source should be straightforward using recursion. Let's write a new test:
```
; tests.lisp
; …
(define-test given-several-number-read-numbers-give-a-list-with-these-numbers
    (let ((result (with-input-from-string (s "42 17 23") (read-numbers s))))
      (assert-equal (list 42 17 23) result)))
```
To make the test pass, we replace the last `nil` in the function, by a recursive call:
```
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
20
3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 5
3
10 10 10
```

Let's try!
```
> rlwrap sbcl --load "bowling"
* (read-numbers (open "lisp/test-cases.txt"))
(5 4 3 5 2 7 6 10 5 4 10 5 2 12 10 10 10 10 10 10 10 10 10 10 10 10 20 3 5 3 5
 3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 10 10 10)
```
It works!
## Printing numbers on the standard output stream
Printing numbers can be done via the powerful function `format`. This function, given a destination of `T` will format and print any value on the standard output. If the destination is `NIL` the result is returned as a string instead of printed.
```
* (format T "~a~%" 4807)
4807
0 (format NIL "~a~%" 42)
"42
"
```
Writing a function `format-numbers` is straighforward, and easier to write than to test, actually.
```
(define-test given-a-list-of-numbers-format-them
    (let ((result (format-numbers NIL (list 4 2))))
      (assert-equal (list (format NIL "~a~%" 4)
                          (format NIL "~a~%" 2)) result)))
```
We create a list of `format`:
```
(defun format-numbers (dest numbers)
  (cond ((null numbers) nil)
        (t (cons
             (format dest "~a~%" (car numbers)) 
             (format-numbers dest (cdr numbers))))))
```
And the test passes.
In fact, we can even simplify this function by using `mapcar` and `lambda`. 

`mapcar` applies a function to each element of a list, and `lambda` creates an anonymous function. Here are examples:
```
* (mapcar #'1+ (list 1 2 3))
(2 3 4)
* ((lambda (n) (* n n)) 42)
1764
* (mapcar (lambda (n) (* n n)) (list 42 17 23))
(1764 289 529)
```
Let's rewrite `format-numbers`:
```
(defun format-numbers (dest numbers)
  (mapcar (lambda (n) (format dest "~a~%" n)) numbers))
```
The tests pass.  We can try our function interactively:
```
> slwrap sbcl --load "bowling"
* (format-numbers T (list 4 8 0 7))
4
8
0
7
(NIL NIL NIL NIL)
```
It works!
