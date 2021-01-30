;
; transforms results of kmest into x,y
; coords for plotting.
;
(provide 'km-plot)
(defun km-plot (time status surv)
  (let* ((n (length time))
         (rep-pat (repeat 2 n))
         (time-plot (cons 0 (repeat time rep-pat)))
         (surv-plot (reverse 
                     (cons (select surv (- n 1)) 
                           (repeat (reverse (cons 1 
                                                  (select 
                                                   surv 
                                                   (iseq (- n 1))))) 
                                   rep-pat)))))
       
    (list time-plot surv-plot)))
