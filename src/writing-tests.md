# Writing Tests

To create tests, we will use the `lisp-unit` library.

The library is available [here:](https://github.com/OdonataResearchLLC/lisp-unit)

A simple way to make it available for our tests scripts is to copy `lips-unit.lisp` in the standard location for lisp libraries:
```
cp lisp-unit.lisp ~/.local/share/common-lisp/source/.```

Here's a test script with a dummmy test in it.
```lisp
; tests.lisp
(require "lisp-unit" "~/.local/share/common-lisp/source/lisp-unit.lisp")
; set up testing option
(in-package :lisp-unit)
(setq *print-failures* t)

(define-test dummy-test
    (assert-equal 5 (+ 2 2)))

(run-tests :all)
```
