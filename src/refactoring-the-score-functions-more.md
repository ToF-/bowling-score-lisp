## Refactoring the Score Functions More

The function still presents repetitions, and can be improved a bit.
```lisp
(defun points (frame rolls)
  (cond ((null rolls) 0)
        ((>= frame 10) 0)
        ((< (length rolls) 2) (car rolls))
        ((is-strike rolls) (+ (add-strike (cdr rolls))
                              (car rolls)
                              (points (1+ frame) (cdr rolls))))
        (t (+ (if (is-spare rolls) (caddr rolls) 0)
              (car rolls)
              (cadr rolls)
              (points (1+ frame) (cddr rolls))))))
```
Since the conditions are ordered in the `points` function, they prevent the helper functions to access illegal elements in the `rolls` list:
```lisp
(defun is-spare (rolls)
  (= 10 (+ (car rolls) (cadr rolls))))

(defun is-strike (rolls)
  (= 10 (car rolls)))
```

