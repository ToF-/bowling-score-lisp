## Reading a Number

Now let's add a new case:
```lisp
; tests.lisp
; …
(define-test given-one-number-read-numbers-give-a-list-with-this-number
    (let ((result (with-input-from-string (s "42") (read-numbers s))))
      (assert-equal (list 42) result)))
```
```
> make unit
…
GIVEN-NO-NUMBERS-READ-numbers-give-NIL: 1 assertions passed, 0 failed.

 | Failed Form: RESULT
 | Expected (42) but saw NIL
 |
```
This fails as expected.

To make our second test pass, reading one number, we have to detect that a numbers has been read from the source, or not. If there was a number, return a list with that number inside, if not, return `nil`.
```lisp
(defun read-numbers (source)
  (let ((n (read source)))
    (if (null n) nil
      (cons n nil))))
```
But now the result is unexpected: our test of empty source is failing.
```
GIVEN-NO-NUMBERS-READ-NUMBERS-GIVE-NIL: 0 assertions passed, 0 failed, and an execution error.
GIVEN-ONE-NUMBER-READ-NUMBERS-GIVE-A-LIST-WITH-THIS-NUMBER: 1 assertions passed, 0 failed.
GIVEN-NO-ROLL-SCORE-IS-ZERO: 1 assertions passed, 0 failed.
GIVEN-ONE-ROLL-SCORE-IS-THAT-ROLL: 1 assertions passed, 0 failed.
GIVEN-TWO-ROLLS-SCORE-IS-THEIR-SUR: 1 assertions passed, 0 failed.

Unit Test Summary
 | 4 assertions total
 | 4 passed
 | 0 failed
 | 1 execution errors
 | 0 missing tests
 ```
