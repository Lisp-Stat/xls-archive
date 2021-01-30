;
; performs Kaplan-Meier estimation.
;
(provide 'kmest)
(defun kmest (time status)
  (let* ((n (length time))
         (s 1)
         (surv nil)         
         (v 0))

    (dotimes (i n)
             (setf s (* s (- 1 (/ (select status i) (- n i)))))
             (setf surv (cons s surv)))
             
    (reverse surv)))
