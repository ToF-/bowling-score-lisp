(defpackage :bowling
(:export :score))

(defun spare? (roll1 roll2)
  (= 10 (+ roll1 roll2))
  )

(defun spare (rolls)
  (cond ((< (length rolls) 3) 0)
        (t (let* ((r1 (car rolls))
                  (r2 (cadr rolls))
                  (bonus (if (spare? r1 r2) (caddr rolls) 0))
                  )
               (+ bonus (spare (cdr (cdr rolls))))
             )
           )
        )
  )

(defun score (rolls)
  (+ (apply '+ rolls)
     (spare rolls))
  )
