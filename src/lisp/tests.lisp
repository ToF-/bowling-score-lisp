; ; load the quicklisp package manager if not already loaded
; #-quicklisp
; (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
;                                        (user-homedir-pathname))))
;   (when (probe-file quicklisp-init)
;     (load quicklisp-init)))
; 
; ; load the unit test library
; (ql:quickload :lisp-unit)

(require "lisp-unit" "~/.local/share/common-lisp/source/lisp-unit.lisp")
; set up testing option
(in-package :lisp-unit)
(setq *print-failures* t)

(load "bowling")

(define-test given-no-numbers-read-numbers-give-nil
    (let ((result (with-input-from-string (s "") (read-numbers s))))
      (assert-equal nil result)))

(define-test given-one-number-read-numbers-give-a-list-with-this-number
    (let ((result (with-input-from-string (s "42") (read-numbers s))))
      (assert-equal (list 42) result)))

(define-test given-several-number-read-numbers-give-a-list-with-these-numbers
    (let ((result (with-input-from-string (s "42 17 23") (read-numbers s))))
      (assert-equal (list 42 17 23) result)))

(define-test processing-one-game-give-a-one-score-list
    (let ((games (list 1 3 5 4 2)))
          (assert-equal (list 11) (process-games games)))
    (let ((games (list 1 4 6 2 3 5)))
          (assert-equal (list 16) (process-games games))))

(define-test processing-two-games-give-a-list-of-two-scores
    (let ((games (list 2 3 5 4 2 2 6 1)))
          (assert-equal (list 11 7) (process-games games))))

(define-test given-no-roll-score-is-zero
    (assert-equal 0 (score ())))

(define-test given-one-roll-score-is-that-roll
    (assert-equal 7 (score (list 7))))

(define-test given-two-rolls-score-is-their-sum
    (assert-equal 8 (score (list 2 6))))

(define-test given-a-spare-on-first-rolls-third-roll-is-added-as-bonus
    (assert-equal 18 (score (list 3 7 4)))
    (assert-equal 28 (score (list 2 8 9))))

(define-test given-a-spare-on-second-frame-next-roll-is-added-as-bonus
    (assert-equal 22 (score (list 4 0 3 7 4))))

(define-test given-a-spare-on-any-frame-next-roll-is-added-as-bonus
    (assert-equal (+ 10 3 10 2 10 9 9) (score (list 4 6 3 7 2 8 9))))

(define-test given-a-strike-on-first-roll-roll-2-and-3-are-added-as-bonus
    (assert-equal 20 (score (list 10 3 2)))
    (assert-equal 28 (score (list 10 8 1))))

(define-test given-a-strike-on-first-roll-with-no-other-roll-then-no-bonus-yet
    (assert-equal 10 (score (list 10))))

(define-test given-a-spare-after-a-strike-then-extra-points-are-counted
    (assert-equal (+ 10 10 3 4 6 3) (score (list 10 4 6 3))))

(define-test after-tenth-frame-extra-rolls-count-only-as-bonus
    (assert-equal 300 (score (list 10 10 10 10 10 10 10 10 10 10 10 10)))
    (assert-equal 276 (score (list 10 10 10 10 10 10 10 10 10 6 4 10))))

(run-tests :all)
