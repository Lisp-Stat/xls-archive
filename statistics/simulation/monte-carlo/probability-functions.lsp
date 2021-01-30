(defun rectangular-dens (x &optional (a 1))
  (cond ((< x 0) 0)
        ((> x a) 0)
        (t (/ a))
        ) 
  )

(defun rectangular-cdf (x &optional (a 1))
  (cond ((< x 0) 0)
        ((> x a) 1)
        (t x)
        ) 
  )

(defun exponential-dens (x &optional (lambda 1))
  (cond ((< x 0) 0)
        (t (* lambda (exp (- (* lambda x)))))
        )
  )

(defun exponential-cdf (x &optional (lambda 1))
  (cond ((< x 0) 0)
        (t (- 1 (exp (- (* lambda x)))))
        )
  )

(defun rayleigh-dens (x &optional (sigma 1))
  (let ((y (/ x sigma)))
    (cond ((< x 0) 0)
          (t (* y (exp ( - (/ (^ y 2) 2)))))
          )
    )
  )

(defun rayleigh-cdf (x &optional (sigma 1))
  (cond ((< x 0) 0)
        (t (- 1 (exp (- (/ (^ (/ x sigma) 2) 2)))))
        )
  )

(defun triangular-dens (x &optional (a 1))
  (cond ((< x 0) 0)
        ((> x a) 0)
        (t (* (/ 2 a) (- 1 (/ x a))))
        )
  )

(defun triangular-cdf (x &optional (a 1))
  (cond ((< x 0) 0)
        ((> x a) 1)
        (t (* (/ 2 a) (- x (/ (^ x 2) (* 2 a)))))
        )
  )

(defun pareto-dens (x &optional (a 1) (b 1))
  (cond ((< x b) 0)
        (t (/ (* a (expt b a)) (expt x (1+ a))))
        )
  )

(defun pareto-cdf (x &optional (a 1) (b 1))
  (cond ((< x b) 0)
        (t (- 1 (expt (/ b x) a)))
        )
  )





