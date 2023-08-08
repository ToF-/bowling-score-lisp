(defpackage :bowling
(:export :score :read-numbers :process-games))

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

(defun extra-points (rolls)
  (cond ((null rolls) 0)
        ((is-strike rolls) (+ (add-strike (cdr rolls)) (extra-points (cdr rolls))))
        ((is-spare rolls) (+ (caddr rolls) (extra-points (cddr rolls))))
        (t (extra-points (cddr rolls)))))

(defun normal-points (rolls)
  (apply '+ rolls))

(defun score (rolls)
  (+ (normal-points rolls) (extra-points rolls)))

(defun process-game (nb-game game-data)
  (if (zerop nb-game) nil
    (let* ((nb-rolls (car game-data))
           (remaining (cdr game-data))
           (rolls (subseq remaining 0 nb-rolls)))
      (cons (score rolls) (process-game (1- nb-game) (subseq remaining nb-rolls))))))

(defun process-games (game-data)
  (process-game (car game-data) (cdr game-data)))

(defconstant NO-EOF-ERROR nil)

(defun read-numbers (source)
  (let ((n (read source NO-EOF-ERROR)))
    (if (null n) nil
      (cons n (read-numbers source)))))

