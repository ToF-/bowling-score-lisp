; score.lisp
(load "bowling")

(format-numbers T 
  (mapcar #'score 
          (extract-games 
                (cdr (read-numbers *standard-input*)))))
