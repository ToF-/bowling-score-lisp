## Bowling Score: Bonus Points

So far, our `score` function is no more than a recursive addition:
```lisp
(defun score (rolls)
  (cond ((null rolls) 0)
        (t (+ (car rolls) (score (cdr rolls))))))
```
We need to add several capacities:

- taking _spares_ into account for bonus points
- taking _strikes_ into account
- discarding rolls after the 10th frame

One improvement that we can make before going on it simplifying this function. After all, so far, calculating the score is only about adding all the rolls.
```lisp
(defun score (rolls)
  (apply '+ rolls))
```
