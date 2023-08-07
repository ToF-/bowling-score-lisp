## Processing One Game

If our input would consist of only one game, then processing the input numbers would yield a list of one score. Here's an example.

```lisp
(define-test processing-one-game-give-a-one-score-list
    (let ((games (list 1 3 5 4 2)))
          (assert-equal (list 11) (process-games games))))
```
We make the test pass with a fake:
```lisp
(defpackage :bowling
(:export :score :read-numbers :process-games))

(defun process-games (game-data)
  (list 11))
```
Let's add another case to this test.
```lisp
(define-test processing-one-game-give-a-one-score-list
    (let ((games (list 1 3 5 4 2)))
          (assert-equal (list 11) (process-games games)))
    (let ((games (list 1 4 6 2 3 5)))
          (assert-equal (list 16) (process-games games))))
```
Now we need to generalize the implementation a bit. If we get rid of the first two numbers, applying the `score` function to the rest of the list is enough to make the test pass.
```lisp
(defun process-games (game-data)
  (list (score (cdr (cdr game-data)))))
```
Let's add new cases for game data with more than one game.


