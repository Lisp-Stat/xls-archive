(defun g1 (a b cos sin sig) (declare (type double-float a))
 (declare (type double-float b)) (declare (type double-float cos))
 (declare (type double-float sin)) (declare (type double-float sig))
 (prog ((yr 0.0d0) (xr 0.0d0) (one 0.0d0) (zero 0.0d0))
  (declare (type double-float yr)) (declare (type double-float xr))
  (declare (type double-float one)) (declare (type double-float zero))
  (comment
   "     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1973 jun 12"
  )
  (comment
   "     to appear in @solving least squares problems@, prentice-hall, 1974"
  )
  (comment "") (comment "")
  (comment "     compute orthogonal rotation matrix..")
  (comment
   "     compute.. matrix   (c, s) so that (c, s)(a) = (sqrt(a**2+b**2))"
  )
  (comment
   "                        (-s,c)         (-s,c)(b)   (   0          )"
  )
  (comment "     compute sig = sqrt(a**2+b**2)")
  (comment "        sig is computed last to allow for the possibility that")
  (comment "        sig may be in the same location as a or b .") (comment "")
  (setf zero 0.0) (setf one 1.0) (if (<= (abs a) (abs b)) (go label10))
  (setf xr (/ b a)) (setf yr (sqrt (+ one (expt xr 2))))
  (setf cos (sign (/ one yr) a)) (setf sin (* cos xr))
  (setf sig (* (abs a) yr)) (go end_label) label10
  (arithmetic-if b (go label20) (go label30) (go label20)) label20
  (setf xr (/ a b)) (setf yr (sqrt (+ one (expt xr 2))))
  (setf sin (sign (/ one yr) b)) (setf cos (* sin xr))
  (setf sig (* (abs b) yr)) (go end_label) label30 (setf sig zero)
  (setf cos zero) (setf sin one) (go end_label) end_label
  (return (values a b cos sin sig))
))

