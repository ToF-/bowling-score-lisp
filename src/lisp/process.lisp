(load "bowling")

(defun process-game (rolls)
  (format T "~a~%" (score rolls)))

(defun split (numbers n)
  (list (subseq numbers 0 n) (subseq numbers n)))

(defun process-games (n numbers)
    (cond ((zerop n) nil)
          (t (let* ((nrolls (car numbers))
                    (pair (split (cdr numbers) (+  nrolls)))
                    (rolls (car pair))
                    (tail (car (cdr pair))))
               (cons (process-game rolls) (process-games (- n 1) tail))))))
               
(defun process (numbers)
  (cond ((null numbers) nil)
        (t (process-games (car numbers) (cdr numbers)))))

(defun read-list ()
  (let ((n (read *standard-input* nil)))
    (if (null n) nil (cons n (read-list)))))

(process (read-list))
