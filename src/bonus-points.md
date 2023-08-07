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
