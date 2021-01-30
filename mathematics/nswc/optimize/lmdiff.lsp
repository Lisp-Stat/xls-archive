(defun lmdiff (fcn m n x fvec epsfcn tol info iwa wa lwa)
 (declare (type fixnum m)) (declare (type fixnum n))
 (declare (type (simple-array float (*)) x))
 (declare (type (simple-array float (*)) fvec)) (declare (type float epsfcn))
 (declare (type float tol)) (declare (type fixnum info))
 (declare (type (simple-array fixnum (*)) iwa))
 (declare (type (simple-array float (*)) wa)) (declare (type fixnum lwa))
 (prog
  ((factor 0.0d0) (ftol 0.0d0) (gtol 0.0d0) (xtol 0.0d0) (zero 0.0d0)
   (maxfev 0) (mode 0) (mp5n 0) (nfev 0) (nprint 0)
  )
  (declare (type float factor)) (declare (type float ftol))
  (declare (type float gtol)) (declare (type float xtol))
  (declare (type float zero)) (declare (type fixnum maxfev))
  (declare (type fixnum mode)) (declare (type fixnum mp5n))
  (declare (type fixnum nfev)) (declare (type fixnum nprint))
  (setq factor 100.0) (setf info 0)
  (if
   (or (<= n 0) (< m n) (< epsfcn zero) (< tol zero)
    (< lwa (+ (+ (* m n) (* 5 n)) m))
   )
   (go label10)
  )
  (setf maxfev (* 200 (+ n 1))) (setf ftol tol) (setf xtol tol)
  (setf gtol zero) (setf mode 1) (setf nprint 0) (setf mp5n (+ m (* 5 n)))
  (multiple-value-setq
   (fcn m n x fvec ftol xtol gtol maxfev epsfcn dummy_var mode factor nprint
    info nfev dummy_var m iwa dummy_var dummy_var dummy_var dummy_var dummy_var
   )
   (lmdif fcn m n x fvec ftol xtol gtol maxfev epsfcn (fref wa 1) mode factor
    nprint info nfev (fref wa (+ mp5n 1)) m iwa (fref wa (+ n 1))
    (fref wa (+ (* 2 n) 1)) (fref wa (+ (* 3 n) 1)) (fref wa (+ (* 4 n) 1))
    (fref wa (+ (* 5 n) 1))
  ))
  (if (= info 8) (setf info 4)) label10
  (return (values fcn m n x fvec epsfcn tol info iwa wa lwa))
))

