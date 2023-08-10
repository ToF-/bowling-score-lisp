# Strikes & Spares

If the first roll knocked all the pins, it's a strike: the two following rolls will be added to the score as supplementary points.
```
(define-test after-a-strike-on-first-frame-next-two-rolls-add-bonus-points
    (assert-equal 28 (score (list 10 5 4))))
```
We can add the strike condition in the `cond`: in that case, add the roll, the next one, and the following one, and proceed with the rest of the rolls.
```
(defun score (rolls)
  (cond ((null rolls) 0)
        ((= 10 (car rolls)) (+ (car rolls)
                               (cadr rolls)
                               (caddr rolls)
                               (score (cdr rolls))))
        (t (+ (car rolls) (score (cdr rolls))))))
```
A problem with this code is that it might break in case the list does not include the following rolls yet. 
Let's add assertions to the test and change is name.
```
(define-test after-a-strike-on-first-frame-next-two-rolls-if-any-add-bonus-points
    (assert-equal 28 (score (list 10 5 4)))
    (assert-equal 20 (score (list 10 5)))
    (assert-equal 10 (score (list 10))))
```
Now we have an execution error, because acessing `(cadr rolls)` when `rolls` has only one element yields 'NIL' and 'NIL' cannot be added to a number. We have to make sure the bonus rolls are values before we add them to the score.
```
(defun score (rolls)
  (cond ((null rolls) 0)
        ((= 10 (car rolls)) (+ (car rolls)
                               (if (not (null (cadr rolls))) (cadr rolls) 0)
                               (if (not (null (caddr rolls))) (caddr rolls) 0)
                               (score (cdr rolls))))
        (t (+ (car rolls) (score (cdr rolls))))))
```
The test pass. Now we can refactor the code a bit.
```
(defun any (x)
     (if (not (null x)) x 0))

(defun score (rolls)
  (cond ((null rolls) 0)
        ((= 10 (car rolls)) (+ (car rolls)
                               (any (cadr rolls))
                               (any (caddr rolls))
                               (score (cdr rolls))))
        (t (+ (car rolls) (score (cdr rolls))))))
```
Let's add the case for a spare in the first frame.
```
(define-test after-a-spare-on-first-frame-next-roll-if-any-add-bonus-points
    (assert-equal 20 (score (list 2 8 5))))
```
To make this test pass, we add a clause to the `cond`:
```
(defun score (rolls)
  (cond ((null rolls) 0)
        ((= 10 (car rolls)) (+ (car rolls)
                               (any (cadr rolls))
                               (any (caddr rolls))
                               (score (cdr rolls))))
        ((= 10 (+ (car rolls) (any (cadr rolls)))) (+ (car rolls)
                                                      (any (cadr rolls))
                                                      (any (caddr rolls))
                                                      (score (cddr rolls))))
        (t (+ (car rolls) (score (cdr rolls))))))
```
Here again, we can refactor.
```
(defun any (x)
     (if (not (null x)) x 0))

(defun frame-plus-bonus (rolls)
  (+ (car rolls) (any (cadr rolls)) (any (caddr rolls))))

(defun strike (rolls)
  (= 10 (car rolls)))

(defun spare (rolls)
  (= 10 (+ (car rolls) (any (cadr rolls)))))

(defun score (rolls)
  (cond ((null rolls) 0)
        ((strike rolls) (+ (frame-plus-bonus rolls) (score (cdr rolls))))
        ((spare rolls) (+ (frame-plus-bonus rolls) (score (cddr rolls))))
        (t (+ (car rolls) (score (cdr rolls))))))
```
Let's add a general test of this function for good measure.
```
(define-test after-some-strikes-and-spares-bonus-rolls-are-added
    (assert-equal 73 (score (list 5 5  4 5  8 2  10  0 10 0 0)))
    (assert-equal 37 (score (list 5 5  4 0  8 1  10  0 0)))
    (assert-equal 151 (score (list 5 5  4 0  8 1  10  0 10  10  10  10  4 6  0 0))))
```
They pass: provided that on illegal game (like e.g {5, 10, 2…}) is given, our score function works… until the 10th frame at least.
