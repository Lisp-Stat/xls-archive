(defun logistic-fn (&optional (theta 1) (gamma 1))
  (let* ((x1 (repeat (iseq 1 10) 10))
         (x2 (sort-data x1))
         (u (+ (* (sin theta) x1) (* (cos theta) x2)))
         (y (+ u (* gamma (/ (exp (- u)) (- 1 (exp (- u)))))))
         (reg (regression-model (list x1 x2) y 
                                :predictor-names '("X1" "X2"))))
        reg))
(def l-reg (logistic-fn))

(send l-reg :graphics-menu "Logistic-fn-data")
