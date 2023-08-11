# Source Code
## tests.lisp
```
; tests.lisp
(require "lisp-unit" "~/.local/share/common-lisp/source/lisp-unit.lisp")
; set up testing options
(in-package :lisp-unit)
(setq *print-failures* t)
(load "bowling")

(define-test when-no-rolls-yet-score-is-zero
    (assert-equal 0 (score ())))

(define-test when-only-one-rolls-score-is-this-roll
    (assert-equal 7 (score (list 7))))

(define-test when-given-average-rolls-score-is-the-sum-of-the-rolls
    (assert-equal 27 (score (list 4 5 3 6 2 7))))

(define-test after-a-strike-on-first-frame-next-two-rolls-if-any-add-bonus-points
    (assert-equal 28 (score (list 10 5 4)))
    (assert-equal 20 (score (list 10 5)))
    (assert-equal 10 (score (list 10))))

(define-test after-a-spare-on-first-frame-next-roll-if-any-add-bonus-points
    (assert-equal 20 (score (list 2 8 5))))

(define-test after-some-strikes-and-spares-bonus-rolls-are-added
    (assert-equal 73 (score (list 5 5  4 5  8 2  10  0 10 0 0)))
    (assert-equal 37 (score (list 5 5  4 0  8 1  10  0 0)))
    (assert-equal 151 (score (list 5 5  4 0  8 1  10  0 10  10  10  10  4 6  0 0))))

(define-test after-tenth-frame-extra-rolls-count-only-as-bonus
    (assert-equal 300 (score (list 10 10 10 10 10 10 10 10 10 10 10 10)))
    (assert-equal 266 (score (list 10 10 10 10 10 10 10 10 10 4 6 2)))
    (assert-equal 262 (score (list 10 10 10 10 10 10 10 10 10 4 5)))
    (assert-equal 264 (score (list 10 10 10 10 10 10 10 10 10 4 6 0))))

(define-test given-no-numbers-read-numbers-give-nil
    (let ((result (with-input-from-string (s "") (read-numbers s))))
      (assert-equal nil result)))

(define-test given-one-number-read-numbers-give-a-list-with-this-number
    (let ((result (with-input-from-string (s "42") (read-numbers s))))
      (assert-equal (list 42) result)))

(define-test given-several-number-read-numbers-give-a-list-with-these-numbers
    (let ((result (with-input-from-string (s "42 17 23") (read-numbers s))))
      (assert-equal (list 42 17 23) result)))

(define-test given-a-list-of-numbers-format-them
    (let ((result (format-numbers NIL (list 4 2))))
      (assert-equal (list (format NIL "~a~%" 4)
                          (format NIL "~a~%" 2)) result)))

(define-test given-game-data-extract-game-return-first-game-and-remaining-data
    (let ((result (extract-game (list 3 4 9 0 2 5 8))))
        (assert-equal '((4 9 0) 2 5 8) result)))

(define-test given-game-data-extract-games-return-all-the-games
    (let ((result (extract-games (list 3 4 9 0 2 5 8))))
        (assert-equal '((4 9 0) (5 8)) result)))
(run-tests :all)
```
## bowling.lisp
```
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
          (t (let* ((extraction (extract-game games))
                    (game (car extraction))
                    (remaining (cdr extraction)))
               (cons game (extract-games remaining))))))

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
        ((strike rolls) (+ (frame-plus-bonus rolls)
                           (score-at-frame (1+ frame) (cdr rolls))))
        ((spare rolls) (+ (frame-plus-bonus rolls)
                          (score-at-frame (1+ frame) (cddr rolls))))
        (t (+ (car rolls) (any (cadr rolls))
              (score-at-frame (1+ frame) (cddr rolls))))))

(defun score (rolls)
  (score-at-frame 0 rolls))
```
### score.lisp
```
; score.lisp
(load "bowling")

(format-numbers T 
  (mapcar #'score 
          (extract-games 
                (cdr (read-numbers *standard-input*)))))
```
