## Processing Two Games

Let's now improve our function to process several games.
```lisp
(define-test processing-two-games-give-a-list-of-two-scores
    (let ((games (list 2 3 5 4 2 2 6 1)))
          (assert-equal (list 11 7) (process-games games))))
```
This test fails, because our function in its current state just ignores the first two elements in the game data and proceed to calculate the score from the rest of the data:
```

 | Failed Form: (PROCESS-GAMES GAMES)
 | Expected (11 7) but saw (20)
 |
 ```
 Let's first get into a stable state again iby cheating with a if and fake return values:
```lisp
(defun process-games (game-data)
  (if (= 1 (car game-data))
    (list (score (cdr (cdr game-data))))
    (list 11 7)))
```
