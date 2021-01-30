(defun lainv (mo fun t_ aerr rerr y c error num ierr)
 (declare (type fixnum mo)) (declare (type double-float t_))
 (declare (type double-float aerr)) (declare (type double-float rerr))
 (declare (type double-float y)) (declare (type double-float c))
 (declare (type double-float error)) (declare (type fixnum num))
 (declare (type fixnum ierr))
 (prog ((ier 0) (a 0.0d0) (ierc 0)) (declare (type fixnum ier))
  (declare (type double-float a)) (declare (type fixnum ierc)) (setf num 0)
  (setf ierc (- 1)) (if (>= ierr 0) (go label10)) (if (= mo 0) (go label5))
  (multiple-value-setq (fun c num ierc) (abcon1 fun c num ierc)) (go label10)
  label5 (multiple-value-setq (fun c num ierc) (abcon fun c num ierc)) label10
  (if (= ierc 2) (go label100)) (setf a (+ c (/ 2.0 t_)))
  (if (> (* a t_) (exparg 0)) (go label110))
  (multiple-value-setq (fun t_ c rerr aerr y error num1 ier)
   (lainv1 fun t_ c rerr aerr y error num1 ier)
  )
  (setf num (+ num num1)) (if (>= ierc 0) (go label20))
  (if (= ier 0) (go label30)) (if (= ier 1) (go label50)) label20
  (if (= ier 2) (go label80)) (if (= ier 1) (go label60))
  (if (= ierc 1) (go label40)) label30 (setf ierr 0) (go end_label) label40
  (setf ierr 1) (go end_label) label50 (setf ierr 2) (go end_label) label60
  (if (= ierc 1) (go label70)) (setf ierr 2) (go end_label) label70
  (setf ierr 3) (go end_label) label80 (setf ierr 4) (go end_label) label100
  (setf y 0.0) (setf error 1.0) (setf ierr 5) (go end_label) label110
  (setf y 0.0) (setf error 1.0) (setf ierr 6) (go end_label) end_label
  (return (values mo fun t aerr rerr y c error num ierr))
))

