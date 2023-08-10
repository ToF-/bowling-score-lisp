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

(run-tests :all)
