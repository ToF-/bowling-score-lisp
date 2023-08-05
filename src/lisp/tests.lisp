(ql:quickload :lisp-unit)
(in-package :lisp-unit)
(setq *print-failures* t)

(load "bowling")

(define-test no-rolls-gives-zero
    (assert-equal 0 (score () )))

(define-test no-spare-nor-strike-gives-sum-of-rolls
    (assert-equal 17 (score (list 3 5 4 3 1 1))))

(define-test after-a-spare-next-roll-is-counted-as-extra-points
    (assert-equal (+ 10 4 4) (score (list 2 8 4))))

(define-test score-includes-all-spare-bonus-points
    (assert-equal (+ 10 4 10 3 3) (score (list 2 8 4 6 3)))
    (assert-equal (+ 7 10 3 3) (score (list 2 5 4 6 3))))


(run-tests :all)
(sb-ext:quit)
