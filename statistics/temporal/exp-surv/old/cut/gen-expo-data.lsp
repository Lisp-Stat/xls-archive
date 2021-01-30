;
; generate data from exponential
; distribution with censoring. censoring
; times are generated as uniform random
; numbers between 0 and time of death.
;
(provide 'gen-expo-data)
(load "exp-rand")
(load "cosort")


(defun gen-expo-data (num1 num2 prob-cen l1 l2)
  (setf time1 (exp-rand (repeat l1 num1)))
  (setf time2 (exp-rand (repeat l2 num2)))
  (setf stat1 (binomial-rand num1 1 (- 1 prob-cen)))
  (setf stat2 (binomial-rand num2 1 (- 1 prob-cen)))
  (mapcar #'(lambda (i) (if (= (elt stat1 i) 0)
                            (setf (select time1 i)
                                  (first (* (elt time1 i) 
                                            (uniform-rand 1))))))
          (iseq (length time1)))
  (mapcar #'(lambda (i) (if (= (elt stat2 i) 0)
                            (setf (select time2 i)
                                  (first (* (elt time2 i)
                                            (uniform-rand 1))))))
          (iseq (length time2)))

  (setf data1 (cosort time1 stat1))
  (setf time1 (elt data1 0))
  (setf stat1 (elt data1 1))
  (setf data2 (cosort time2 stat2))
  (setf time2 (elt data2 0))
  (setf stat2 (elt data2 1))
  nil)
