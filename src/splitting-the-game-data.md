## Splitting the Game Data

Here's our function `process-games` in its current state: 
```lisp
(defun process-games (game-data)
  (if (= 1 (car game-data))
    (list (score (cdr (cdr game-data))))
    (list 11 7)))
```
Where does this `11` comes from ? From calculating the score of _some_ elements of the list given by the test: `(list 2 3 5 4 2 2 6 1)`.

Which elements ? 5, 4 and 2, that is elements 2 to 5 (counting from 0).

How can we extract these elements specifically ? By using `subseq` : this function returns a copy of a subsequence of a list starting with an element number until the end of the list of until a specified element number.

Let's try it.
```lisp
> rlwrap sbcl
* (defvar l '(A B C D E F G H I J))
L
* (subseq l 2)
(C D E F G H I J)
* (subseq l 2 5)
(C D E)
```
This is what we need. Let's use it in our function:
```lisp
(defun process-games (game-data)
  (if (= 1 (car game-data))
    (list (score (subseq game-data 2)))
    (list (score (subseq game-data 2 5))
          (score (subseq game-data 6)))))
```
The tests pass. Still a lot of cheating, but a little less.
Let's try to pass the _game number_ as an argument, by extracting a function.
```lisp
(defun process-game (nb-game game-data)
  (if (= 1 nb-game)
    (list (score (subseq game-data 2)))
    (list (score (subseq game-data 2 5))
          (score (subseq game-data 6)))))

(defun process-games (game-data)
  (process-game (car game-data) game-data))
```
We can eliminate the number of games from the _game-data_ argument.
```lisp
(defun process-game (nb-game game-data)
  (if (= 1 nb-game)
    (list (score (subseq game-data 1)))
    (list (score (subseq game-data 1 4))
          (score (subseq game-data 5)))))

(defun process-games (game-data)
  (process-game (car game-data) (cdr game-data)))
```