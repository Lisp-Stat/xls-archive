
(def rat-data (read-data-columns "rat.data" 7))
(def y (combine (mapcar #'rest
                    (select rat-data (iseq 1 5)))))
(def rat (repeat (iseq 10) 5))
(def intercept (repeat 1 50))
(def week (repeat (iseq 5) (repeat 10 5)))
(def x (list intercept week))
(def z (rest (select rat-data 6)))

(def unit-data (bind-columns (iseq 1 10) z))
(def case-data (bind-columns (repeat (iseq 1 10) (repeat 5 10))
                             (repeat (iseq -2 2) 10)
                             y))

(load "ter2")

(setf rat (make-terrace unit-data case-data
            :x-labels (list "Intercept" "Week")
            :z-labels (list "Intercept" "MothersWgt")))

;(load "terrace")
;(setf rat (terrace-mlr-model x y rat z '((0 1) (0 1)) '(0 1)
;                    :var-labels '("Intercept" "Week")))

