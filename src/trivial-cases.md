# Trivial Cases

## A Todo List

These are the cases I can think of so far :

- simple, average rolls, not making bonus points
- bonus after a spare (two rolls adding up to 10)
- two rolls adding up to 10 don't necessarily make a spare
- bonuses after a strike
- several strikes in a row
- bonuses from the last frame

## Games that don't make bonus points:e 

Our very first case will be about what to expect when there are no rolls in the game yet:
```lisp
(load "bowling")

(define-test no-rolls-gives-zero
    (assert-equal 0 (score () )))
```

The function `score`, defined in the script `bowling` will just have to return 0 to make this test pass. Here's the script `bowlin.lisp` :
```lisp
(defpackage :bowling
(:export :score))

(defun score (rolls)
  0)
```

In a game where there are no strikes nor spares, there are no bonus, so the score amounts to the sum of the rolls:

```lisp
(define-test no-spare-nor-strike-gives-sum-of-rolls
    (assert-equal 17 (score (list 3 5 4 3 1 1))))
```

To make this test and the previous one pass, we can use recursion:
```lisp
(defun score (rolls)
  (cond ((null rolls) 0)
        (t (+
             (car rolls)
             (score (cdr rolls)))
           )
        )
  )
```
But applying the `+` function to the list of rolls is even simpler:
```lisp
(defun score (rolls)
  (apply '+ rolls))
```

