(defun abcon (fun c num ierr) (declare (type double-float c))
 (declare (type fixnum num)) (declare (type fixnum ierr))
 (prog ((find nil) (xmin 0.0d0) (eta 0.0d0)) (declare (type t find))
  (declare (type double-float xmin)) (declare (type double-float eta))
  (setf eta 0.01) (setf xmin (- 10073.6))
  (multiple-value-setq (fun xcond xmin eta x0 num ierr)
   (srch fun xcond xmin eta x0 num ierr)
  )
  (setf c x0) (if (= ierr 2) (go label20)) (if (= ierr 3) (go label10))
  (multiple-value-setq (fun x0 find num1 ierr) (acond fun x0 find num1 ierr))
  (setf num (+ num num1)) (if find (go label20)) label10
  (multiple-value-setq (fun acond x0 eta c num1 ierr)
   (srch fun acond x0 eta c num1 ierr)
  )
  (setf num (+ num num1)) (if (= ierr 3) (setf c 0.0)) label20
  (setf ierr (min0 ierr 2)) (return (values fun c num ierr))
))

