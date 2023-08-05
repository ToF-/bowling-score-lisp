# Counting spare bonus points

When the pins knocked down in the previous two rolls add up to ten, then the next roll has to be counted as a bonus. Let's write a test showing that case:
```lisp
(define-test after-a-spare-next-roll-is-counted-as-extra-points
    (assert-equal (+ 10 4 4) (score (list 2 8 4))))
```

The `spare` function will examine the two first rolls and return the third roll, if there are at least 3 rolls, and the first two rolls make a spare. Otherwise, it will return 0.

Adding the evaluation of this function to the score computation will make our test pass:

```lisp
(defun spare (rolls)
  (if (>= (length rolls) 3)
    (if (= 10 (+ (car rolls) (cadr rolls)))
        (caddr rolls)
        0)
    0)
  )

(defun score (rolls)
  (+ (apply '+ rolls)
     (spare rolls))
  )
```
There can be several spare frames in a game. Let's write a new test:
```lisp
(define-test score-includes-all-spare-bonus-points
    (assert-equal (+ 10 4 10 3 3) (score (list 2 8 4 6 3))))
    (assert-equal (+ 10 4 4 10 3 3) (score (list 2 8 4 6 3))))
```
To make it pass, we have to make our `spare` function a recursive function:
```lisp
(defun spare (rolls)
  (if (>= (length rolls) 3)
    (if (= 10 (+ (car rolls) (cadr rolls)))
        (+ (caddr rolls) (spare (cdr (cdr rolls))))
        0)
    0)
  )
```
Refactoring a bit:
```lisp
(defun spare? (roll1 roll2)
  (= 10 (+ roll1 roll2))
  )

(defun spare (rolls)
  (cond ((< (length rolls) 3) 0)
        (t (let* ((r1 (car rolls))
                  (r2 (cadr rolls))
                  (r3 (caddr rolls))
                  )
             (if (spare? r1 r2)
               (+ r3 (spare (cdr (cdr rolls))))
               0)
             )
           )
        )
  )

(defun score (rolls)
  (+ (apply '+ rolls)
     (spare rolls))
  )
```
Our function is not correct for the cases where the spare is not in the first frame. Let's complement our test with another assertion:
```lisp
(define-test score-includes-all-spare-bonus-points
    (assert-equal (+ 10 4 10 3 3) (score (list 2 8 4 6 3)))
    (assert-equal (+ 7 10 3 3) (score (list 2 5 4 6 3))))
```
Let's fix the function:
```lisp
(defun spare (rolls)
  (cond ((< (length rolls) 3) 0)
        (t (let* ((r1 (car rolls))
                  (r2 (cadr rolls))
                  (r3 (caddr rolls))
                  )
             (if (spare? r1 r2)
               (+ r3 (spare (cdr (cdr rolls))))
               (spare (cdr (cdr rolls)))
               )
             )
           )
        )
  )
```
This can be refactored to remove some duplication:
```lisp
(defun spare (rolls)
  (cond ((< (length rolls) 3) 0)
        (t (let* ((r1 (car rolls))
                  (r2 (cadr rolls))
                  (bonus (if (spare? r1 r2) (caddr rolls) 0))
                  )
               (+ bonus (spare (cdr (cdr rolls))))
             )
           )
        )
  )
```
