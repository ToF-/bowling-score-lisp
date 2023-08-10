; bowling.lisp
(defpackage :bowling
(:export :score))

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
