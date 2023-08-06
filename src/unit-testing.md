## Unit testing

To run unit tests, we create a script file named `tests.lisp` for instance, and write some tests:
```lisp
; load the quicklisp package manager if not already loaded
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

; load the unit test library
(ql:quickload :lisp-unit)

; set up testing option
(in-package :lisp-unit)
(setq *print-failures* t)

; a first test
(define-test dummy-test
    (assert-equal 5 (+ 2 2)))

(run-tests :all)
```
Since we will run this script via `sbcl --noscript tests.lisp`, we need to make sure `quicklisp` is loaded before this import:
```lisp
(qu:quickload :lisp-unit)
```

### running the tests

To run the tests, execute the script
```
> sbcl --script tests.lisp
To load "lisp-unit":
  Load 1 ASDF system:
    lisp-unit
; Loading "lisp-unit"

 | Failed Form: (+ 2 2)
 | Expected 5 but saw 4
 |
DUMMY-TEST: 0 assertions passed, 1 failed.

Unit Test Summary
 | 1 assertions total
 | 0 passed
 | 1 failed
 | 0 execution errors
 | 0 missing tests
 ```

Of course this test was intentionally programmed to fail.
Let's make the test pass and run the script again:
```lisp
(define-test dummy-test
    (assert-equal 4 (+ 2 2)))
```
```
> sbcl  --script tests.lisp
To load "lisp-unit":
  Load 1 ASDF system:
    lisp-unit
; Loading "lisp-unit"

DUMMY-TEST: 1 assertions passed, 0 failed.

Unit Test Summary
 | 1 assertions total
 | 1 passed
 | 0 failed
 | 0 execution errors
 | 0 missing tests
