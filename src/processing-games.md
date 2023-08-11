## Processing the Games in Input

As explained initially our program's task is to:

- read the input stream, taking all the numbers from there
- interpret this list of numbers as _information_ about the test cases:
    - a number _T_ of tests cases followed by _T_ series of
        - a number _N_ of rolls in this test case, followed by
        - _R1_ _R2_ … _RN_ numbers: the rolls themselves for this test case
- produce the scores for these games,
- print each of these scores, separated by a new line.

In this chapter, we will interest ourselves in the main function of our program. It will consume a list, and return a list.

For instance, processing the list `(3 2 8 0 4 8 1 7 2 5 8 1 7 2 6)` should result the list `(8 18 24)`. Why?

- the first number indicates 3 games to process. The data for these 3 games is formed by the list `(2 8 0 4 8 1 7 2 5 8 1 7 2 6)`.
- the first number the first game has 2 rolls: `(8 0)` for which computing the score will yield `8`. The rest to process is the list `(4 8 1 7 2 5 8 1 7 2 6)`.
- the second game has 4 rolls: `(8 1 7 2)` for which computing the score will yield `18`. The rest to process it the list `(5 8 1 7 2 6)`
- the third game has 5 rolls: `(8 1 7 2 6)` for a score of `24` and the rest to process is the empty list, which will end the process.

### Extracting the first game from the game data
In order to process each game in turn, we need to generate 2 lists:
- the rolls for the current game to be processed (the lenght of this list is given by the very firt element of the data)
- the rest of game data 

Here's test:
```
(define-test given-game-data-extract-game-return-first-game-and-remaining-data
    (let ((result (extract-game (list 3 4 9 0 2 5 8))))
        (assert-equal '((4 9 0) 2 5 8) result)))
```
We can write this function using `subseq`, which given a list, a start index and an (optional) end index, returns the corresponding subsequence of the list. Here are some examples:
```
* (defvar l '(a b c d e))
L
* (subseq l 0 3)
(A B C)
* (subseq l 3)
(D E)
```
Hence the function:
```
(defun extract-game (games)
  (let* ((n (car games))
         (data (cdr games))
         (rolls (subseq data 0 n))
         (remain (subseq data n)))
    (cons rolls remain)))
```
And the test passes.

### Extracting all the games
From here, extracting all the games is easy: recursively extract each first game. 
Here's a test:
```
(define-test given-game-data-extract-games-return-all-the-games
    (let ((result (extract-games (list 3 4 9 0 2 5 8))))
        (assert-equal '((4 9 0) (5 8)) result)))
```
```
(defun extract-games (games)
    (cond ((null games) nil)
          (t (let* ((extraction (extract-game games))
                    (game (car extraction))
                    (remaining (cdr extraction)))
               (cons game (extract-games remaining))))))
```
We can try the function on the test cases. We must not forget to remove the first number in this list, which represents the number of test cases. This number is not needed, as we rely on the end of the list to stop the process.
```
> sbcl --load bowling.lisp"
* (defvar data (read-numbers (open "./lisp/test-cases.txt")))
DATA
* data
(5 4 3 5 2 7 6 10 5 4 10 5 2 12 10 10 10 10 10 10 10 10 10 10 10 10 20 3 5 3 5
 3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 10 10 10)
* (extract-games (cdr data))
((3 5 2 7) (10 5 4 10 5 2) (10 10 10 10 10 10 10 10 10 10 10 10)
 (3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 5 3 5) (10 10 10))
```
It works!

