; read-list.lisp
(defun read-list ()
  (let ((n (read *standard-input* nil)))
    (if (null n) nil (cons n (read-list)))))

; (format t "~a~%" (read-list))
(format t "~a~%" (apply #'* (read-list)))
