(defun read-list ()
  (let ((n (read *standard-input* nil)))
    (if (null n)
        nil
        (cons n (read-list)))))
(format t "~a~%" (read-list))
(sb-ext:exit)
