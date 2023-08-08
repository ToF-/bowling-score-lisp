(load "bowling")
(mapcar
  (lambda (n) (format T "~a~%" n)) 
  (process-games (read-numbers *standard-input*)))
