(defpackage :bowling
(:export :score :read-numbers :process-games))

(defun process-games (game-data)
  (list (score (cdr (cdr game-data)))))

(defun score (rolls)
  (cond ((null rolls) 0)
        (t (+ (car rolls) (score (cdr rolls))))))

(defconstant NO-EOF-ERROR nil)

(defun read-numbers (source)
  (let ((n (read source NO-EOF-ERROR)))
    (if (null n) nil
      (cons n (read-numbers source)))))

