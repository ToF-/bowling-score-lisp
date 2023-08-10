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

(run-tests :all)
