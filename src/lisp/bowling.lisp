(defpackage :bowling
(:export :score :read-numbers :process-games))

(defun score (rolls)
  (cond ((null rolls) 0)
        (t (+ (car rolls) (score (cdr rolls))))))

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

