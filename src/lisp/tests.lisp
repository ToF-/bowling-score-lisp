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

(define-test given-a-spare-on-first-rolls-third-roll-is-added-for-bonus
    (assert-equal 18 (score (list 3 7 4))))

(run-tests :all)
