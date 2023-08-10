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

(defun score-at-frame (frame rolls)
  (cond ((null rolls) 0)
        ((>= frame 10) 0)
        ((strike rolls) (+ (frame-plus-bonus rolls) (score-at-frame (1+ frame) (cdr rolls))))
        ((spare rolls) (+ (frame-plus-bonus rolls) (score-at-frame (1+ frame) (cddr rolls))))
        (t (+ (car rolls) (any (cadr rolls))  (score-at-frame (1+ frame) (cddr rolls))))))

(defun score (rolls)
  (score-at-frame 0 rolls))
