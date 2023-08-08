(defpackage :bowling
(:export :score :read-numbers :process-games))

(defun is-spare (rolls)
  (= 10 (+ (car rolls) (cadr rolls))))

(defun is-strike (rolls)
  (= 10 (car rolls)))

(defun add-strike (rolls)
  (cond ((null rolls) 0)
        ((< (length rolls) 2) (car rolls))
        (t (+ (car rolls) (cadr rolls)))))

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

(defun score (rolls)
  (+ (points 0 rolls)))

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

