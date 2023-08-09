; tests.lisp
(require "lisp-unit" "~/.local/share/common-lisp/source/lisp-unit.lisp")
; set up testing option
(in-package :lisp-unit)
(setq *print-failures* t)

(define-test dummy-test
    (assert-equal 5 (+ 2 2)))

(run-tests :all)
