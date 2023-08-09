# Writing Tests

## Unit Tests
To create unit tests in lisp, we will use the `lisp-unit` library.

The library is available [here:](https://github.com/OdonataResearchLLC/lisp-unit)

A simple way to make it available for our tests scripts is to copy `lips-unit.lisp` in the standard location for lisp libraries:
```
cp lisp-unit.lisp ~/.local/share/common-lisp/source/.
```

(Or any better location).

Here's a test script with a dummmy test in it.
```
; tests.lisp
(require "lisp-unit" "~/.local/share/common-lisp/source/lisp-unit.lisp")
; set up testing option
(in-package :lisp-unit)
(setq *print-failures* t)

(define-test dummy-test
    (assert-equal 5 (+ 2 2)))

(run-tests :all)
```
When we run this script, we get a test report:
```
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
## Acceptance Tests
The way we are going to create acceptance tests is very straightforward:
- we'll create a `test-cases.txt` file and fill it with a series of test cases
- we'll create a reference file `expected.txt` which will contain the expected results for the test cases
- to test the program, we will run it, feeding it with the acceptance test cases file, and capturing the output in a `results.txt` file
- then will compare `expected.txt` and `results.txt` for an exact match.

This is our test cases file: `test-cases.txt`

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
And this is the expected result file: `expected.txt`
```
17
52
300
80
30
```

