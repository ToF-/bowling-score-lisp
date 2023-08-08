## Refactoring the Score Functions
The code works, but it's pretty complicated:
```lisp
(defun is-spare (rolls)
  (cond ((< (length rolls) 3) nil)
        (t (= 10 (+ (car rolls) (cadr rolls))))))

(defun is-strike (rolls)
  (cond ((< (length rolls) 1) nil)
        (t (= 10 (car rolls)))))

(defun add-strike (rolls)
  (cond ((null rolls) 0)
        ((< (length rolls) 2) (car rolls))
        (t (+ (car rolls) (cadr rolls)))))

(defun extra-points (frame rolls)
  (cond ((null rolls) 0)
        ((>= frame 10) 0)
        ((is-strike rolls) (+ (add-strike (cdr rolls)) (extra-points (1+ frame) (cdr rolls))))
        ((is-spare rolls) (+ (caddr rolls) (extra-points (1+ frame) (cddr rolls))))
        (t (extra-points (1+ frame) (cddr rolls)))))

(defun normal-points (frame rolls)
  (cond ((null rolls) 0)
        ((>= frame 10) 0)
        ((is-strike rolls) (+ 10 (normal-points (1+ frame) (cdr rolls))))
        ((> (length rolls) 1) (+ (car rolls) (cadr rolls) (normal-points (1+ frame) (cddr rolls))))
        (t (car rolls))))

(defun score (rolls)
  (+ (normal-points 0 rolls) (extra-points 0 rolls)))
```
It's time to merge these two aspect of the function again.
```lisp
(defun points (frame rolls)
  (cond ((null rolls) 0)
        ((>= frame 10) 0)
        ((< (length rolls) 2) (car rolls))
        ((is-strike rolls) (+ 10 
                              (add-strike (cdr rolls)) 
                              (points (1+ frame) (cdr rolls))))
        ((is-spare rolls) (+ 10 
                             (caddr rolls)
                             (points (1+ frame) (cddr rolls))))
        (t (+ (car rolls) (cadr rolls)
              (points (1+ frame) (cddr rolls))))))

(defun score (rolls)
  (points 0 rolls))
```
