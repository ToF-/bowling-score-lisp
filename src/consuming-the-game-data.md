# Consuming the Game Data One Game After Another

Let's look at the code again:
```lisp
(defun process-game (nb-game game-data)
  (if (= 1 nb-game)
    (list (score (subseq game-data 1)))
    (list (score (subseq game-data 1 4))
          (score (subseq game-data 5)))))

(defun process-games (game-data)
  (process-game (car game-data) (cdr game-data)))
```
We can eliminate these hard-coded values, by extracting them from the data itself:
```lisp
(defun process-game (nb-game game-data)
  (let* ((nb-rolls (car game-data))
         (rolls (subseq game-data 1 (1+ nb-rolls))))
    (if (= 1 nb-game)
      (list (score rolls))
      (list (score rolls)
            (score (subseq game-data (+ 2 nb-rolls)))))))
```
Let's add more definitions for clarity.
```lisp
(defun process-game (nb-game game-data)
  (let* ((nb-rolls (car game-data))
         (remaining (cdr game-data))
         (rolls (subseq remaining 0 nb-rolls)))
    (if (= 1 nb-game)
      (list (score rolls))
      (list (score rolls)
            (score (subseq remaining (1+ nb-rolls)))))))
```
The test still pass. Now we are ready to introduce recursion.
```lisp
(defun process-game (nb-game game-data)
  (if (zerop nb-game) nil
    (let* ((nb-rolls (car game-data))
           (remaining (cdr game-data))
           (rolls (subseq remaining 0 nb-rolls)))
      (cons (score rolls) (process-game (1- nb-game) (subseq remaining nb-rolls))))))
```
And now we can process any number of games:
```lisp
> sbcl --load "bowling.lisp"
* (defvar s (open "./test-cases.txt"))
S
* (defvar d (read-numbers s))
D
* d
(5 4 3 5 2 7 6 10 5 4 10 5 2 12 10 10 10 10 10 10 10 10 10 10 10 10 10 3 5 3 5
 3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 10 10 10)
* (process-games d)
(17 36 120 40 13)
```
Of course our  `score` function is still wildy incorrect, but we have something working, at least for average games!
