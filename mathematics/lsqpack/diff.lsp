(defun diff (x y) (declare (type double-float x))
 (declare (type double-float y))
 (prog ((diff 0.0d0)) (declare (type double-float diff))
  (comment
   "     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1973 june 7"
  )
  (comment
   "     to appear in @solving least squares problems@, prentice-hall, 1974"
  )
  (setf diff (+ x (- y))) (return diff)
))

