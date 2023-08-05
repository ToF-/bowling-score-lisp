(ql:quickload :lisp-unit)
(in-package :lisp-unit)
(setq *print-failures* t)

(define-test dummy-test
    (assert-equal 5 (+ 3 2)))

(run-tests :all)
(sb-ext:quit)
