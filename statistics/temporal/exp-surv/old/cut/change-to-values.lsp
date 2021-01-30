(provide 'change-to-values)

(defun CHANGE-TO-VALUES (strig)
  
  (let* ((len (length strig))
        (arr (repeat 0 len))
        (digits (^ 10 (- len 1))))

    (dotimes (i len)
             (setf (elt arr  i) (char-code (char strig i))))
  
    (def var 0) 
    (dotimes (i len)
         (setf var (+ var (* (- (elt arr i) 48) digits)))
         (setf digits (/ digits 10)))) var)
    
