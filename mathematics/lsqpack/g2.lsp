(defun g2 (cos sin x y) (declare (type double-float cos))
 (declare (type double-float sin)) (declare (type double-float x))
 (declare (type double-float y))
 (prog ((xr 0.0d0)) (declare (type double-float xr))
  (comment
   "     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1972 dec 15"
  )
  (comment
   "     to appear in @solving least squares problems@, prentice-hall, 1974"
  )
  (comment "          apply the rotation computed by g1 to (x,y).")
  (setf xr (+ (* cos x) (* sin y))) (setf y (+ (* (* -1 sin) x) (* cos y)))
  (setf x xr) (return (values cos sin x y))
))

