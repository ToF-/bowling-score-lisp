(load "bowling")

(defun process-game (rolls)
  (format T "~a~%" (score rolls)))

(defun process-games (n numbers)
  (if (zerop n) nil 
    (let* ((nrolls (car numbers))
           (rolls (subseq numbers 0 nrolls))
           (tail  (subseq numbers nrolls)))
           (cons (process-game rolls) (process-games (1- n) tail)))))

(defun process (numbers)
  (process-games (car numbers) (cdr numbers)))

(defun read-list ()
  (let ((n (read *standard-input* nil)))
    (if (null n) nil (cons n (read-list)))))

(process (read-list))
