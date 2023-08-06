; load the quicklisp package manager if not already loaded
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

; load the unit test library
(ql:quickload :lisp-unit)

; set up testing option
(in-package :lisp-unit)
(setq *print-failures* t)

(load "bowling")

(define-test given-no-roll-score-is-zero
    (assert-equal 0 (score ())))

(define-test given-one-roll-score-is-that-roll
    (assert-equal 7 (score (list 7))))

(define-test given-two-rolls-score-is-their-sur
    (assert-equal 8 (score (list 2 6))))

(run-tests :all)
