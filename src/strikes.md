# Strikes

If the player knocks down all ten pins on their first roll, it's a strike. Then the next 2 rolls are added to the score as a bonus : 

```
(define-test after-a-strike-on-first-frame-next-2-rolls-are-counted-as-bonus
    (assert-equal 20 (score (list 10 3 2))))
```

Let's make the test pass with a *fake* implementation:

```
(defun score (rolls)
  (cond ((equal (list 10 3 2) rolls) 20)
        (t (apply #'+ rolls))))
```

Then let's add another assertion and triangulate:

```
(define-test after-a-strike-on-first-frame-next-2-rolls-are-counted-as-bonus
    (assert-equal 20 (score (list 10 3 2)))
    (assert-equal 24 (score (list 10 4 3))))
```
We can still *fake it*:
```
(defun score (rolls)
  (cond ((equal (list 10 3 2) rolls) 20)
        ((equal (list 10 4 3) rolls) 24)
        (t (apply #'+ rolls))))
```
And then refactor, progressively bringing the general solution by removing the hard coded result values :
```
(defun score (rolls)
  (cond ((equal (list 10 3 2) rolls)
            (+ 10 (cadr rolls) (caddr rolls) (cadr rolls) (caddr rolls)))
        ((equal (list 10 4 3) rolls) 
            (+ 10 (cadr rolls) (caddr rolls) (cadr rolls) (caddr rolls)))
        (t (apply #'+ rolls))))
```
And now we can remove the hard-coded conditions:
```
(defun score (rolls)
  (cond ((eql 10 (car rolls))
            (+ 10 (cadr rolls) (caddr rolls) (cadr rolls) (caddr rolls)))
        (t (apply #'+ rolls))))
```
We can see that this solution works only for a strike on the first frame. What happens in the case of two strikes on first and second frames ?

```
(define-test after-two-strikes-the-next-two-rolls-are-counted-as-bonus
    (assert-equal (+ 23 15 5) (score (list 10 10 3 2))))
```
The test fails because the strike bonus result for the second 10 doesn't get counted. A recursive call to the `score` function with the tail of the list will solve this.
```
(defun score (rolls)
  (cond ((eql 10 (car rolls))
            (+ 10 (cadr rolls) (caddr rolls) (score (cdr rolls))))
        (t (apply #'+ rolls))))
```
What if we have an incomplete input list with a 10 followed by only one roll? Then the rolls should be just be added:
```
(define-test when-two-rolls-score-is-the-sum-of-rolls
    (assert-equal 9 (score (list 4 5)))
    (assert-equal 12 (score (list 10 2))))
```
This creates an execution error, because the list start with a 10, but there's no third value in the list for the `caddr`, so we end up adding `NIL` to the score, which is not allowed.

We can solve this problem by changing the `cond` so that lists of two values or less are checked first:
```
(defun score (rolls)
  (cond ((< (length rolls) 3) (apply #'+ rolls))
        ((eql 10 (car rolls))
         (+ 10 (cadr rolls) (caddr rolls)
                (score (cdr rolls))))))
```
Now what if we have more that 2 rolls in the list but the list is not starting with a 10 ? Let's add a test case:

```
(define-test after-a-normal-frame-rolls-are-added
    (assert-equal (12 (score (list 3 4 1 4)))))
```

The test fails because the `cond` doesn't contain a case for a list of more than 2 rolls not starting with 10. Let's add a default clause that adds the two first rolls and recursively calls the function with the remaining rolls.
```
(defun score (rolls)
  (cond ((< (length rolls) 3) (apply #'+ rolls))
        ((eql 10 (car rolls))
         (+ 10 (cadr rolls) (caddr rolls)
                (score (cdr rolls))))
        (t (+ (car rolls) (cadr rolls)
                (score (cddr rolls))))))
```
