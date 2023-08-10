; bowling.lisp
(defpackage :bowling
(:export :score :read-numbers))

(defconstant NO-EOF-ERROR nil)

(defun extract-game (games)
  (let* ((n (car games))
         (data (cdr games))
         (rolls (subseq data 0 n))
         (remain (subseq data n)))
    (cons rolls remain)))

(defun extract-games (games)
    (cond ((null games) nil)
          (t (let ((extraction (extract-game games)))
               (cons (car extraction) (extract-games (cdr extraction)))))))
                   
                
(defun read-numbers (source)
  (let ((n (read source NO-EOF-ERROR)))
    (if (null n) nil
      (cons n (read-numbers source)))))

(defun format-numbers (dest numbers)
  (mapcar (lambda (n) (format dest "~a~%" n)) numbers))

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
