(defun cosqb (n x wsave) (declare (type fixnum n))
 (declare (type (simple-array double-float (*)) x))
 (declare (type (simple-array double-float (*)) wsave))
 (prog ((tsqrt2 0.0d0) (x1 0.0d0)) (declare (type double-float tsqrt2))
  (declare (type double-float x1)) (setq tsqrt2 2.828427)
  (arithmetic-if (+ n (- 2)) (go label101) (go label102) (go label103))
  label101 (fset (fref x 1) (* 4.0 (fref x 1))) (go end_label) label102
  (setf x1 (* 4.0 (+ (fref x 1) (fref x 2))))
  (fset (fref x 2) (* tsqrt2 (+ (fref x 1) (- (fref x 2)))))
  (fset (fref x 1) x1) (go end_label) label103
  (multiple-value-setq (n x wsave dummy_var)
   (cosqb1 n x wsave (fref wsave (+ n 1)))
  )
  (go end_label) end_label (return (values n x wsave))
))

