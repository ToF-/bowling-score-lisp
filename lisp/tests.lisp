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

(define-test when-two-rolls-score-is-the-sum-of-rolls
    (assert-equal 9 (score (list 4 5))))

; (define-test when-given-average-rolls-score-is-the-sum-of-the-rolls
;     (assert-equal 27 (score (list 4 5 3 6 2 7))))
; 
; 
; (define-test after-a-strike-the-two-next-rolls-are-added-as-bonus
;     (assert-equal 20 (score (list 10 3 2)))
;     (assert-equal 24 (score (list 10 4 3)))
;     (assert-equal 36 (score (list 4 2 10 5 5)))
;     (assert-equal 74 (score (list 10 10 10 4 1))))
; 
; 
; (define-test incomplete-frames-only-add-given-rolls
;     (assert-equal 10 (score (list 10)))
;     (assert-equal 15 (score (list 10 5))))
; 
; (define-test after-a-spare-the-next-roll-is-added-as-bonus
;     (assert-equal 22 (score (list 0 10 5 2)))
;     (assert-equal 17 (score (list 6 4 2 3))))
; 
; (define-test after-a-strike-on-first-frame-next-two-rolls-if-any-add-bonus-points
;     (assert-equal 28 (score (list 10 5 4)))
;     (assert-equal 20 (score (list 10 5)))
;     (assert-equal 10 (score (list 10))))
; 
; (define-test after-a-spare-on-first-frame-next-roll-if-any-add-bonus-points
;     (assert-equal 20 (score (list 2 8 5))))
; 
; (define-test after-some-strikes-and-spares-bonus-rolls-are-added
;     (assert-equal 73 (score (list 5 5  4 5  8 2  10  0 10 0 0)))
;     (assert-equal 37 (score (list 5 5  4 0  8 1  10  0 0)))
;     (assert-equal 151 (score (list 5 5  4 0  8 1  10  0 10  10  10  10  4 6  0 0))))
; 
; (define-test after-tenth-frame-extra-rolls-count-only-as-bonus
;     (assert-equal 300 (score (list 10 10 10 10 10 10 10 10 10 10 10 10)))
;     (assert-equal 266 (score (list 10 10 10 10 10 10 10 10 10 4 6 2)))
;     (assert-equal 262 (score (list 10 10 10 10 10 10 10 10 10 4 5)))
;     (assert-equal 264 (score (list 10 10 10 10 10 10 10 10 10 4 6 0))))
; 
; (define-test given-no-numbers-read-numbers-give-nil
;     (let ((result (with-input-from-string (s "") (read-numbers s))))
;       (assert-equal nil result)))
; 
; (define-test given-one-number-read-numbers-give-a-list-with-this-number
;     (let ((result (with-input-from-string (s "42") (read-numbers s))))
;       (assert-equal (list 42) result)))
; 
; (define-test given-several-number-read-numbers-give-a-list-with-these-numbers
;     (let ((result (with-input-from-string (s "42 17 23") (read-numbers s))))
;       (assert-equal (list 42 17 23) result)))
; 
; (define-test given-a-list-of-numbers-format-them
;     (let ((result (format-numbers NIL (list 4 2))))
;       (assert-equal (list (format NIL "~a~%" 4)
;                           (format NIL "~a~%" 2)) result)))
; 
; (define-test given-game-data-extract-game-return-first-game-and-remaining-data
;     (let ((result (extract-game (list 3 4 9 0 2 5 8))))
;         (assert-equal '((4 9 0) 2 5 8) result)))
; 
; (define-test given-game-data-extract-games-return-all-the-games
;     (let ((result (extract-games (list 3 4 9 0 2 5 8))))
;         (assert-equal '((4 9 0) (5 8)) result)))
(run-tests :all)
