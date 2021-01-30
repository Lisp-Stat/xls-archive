(defun rmin (n m cond unitrd averr) (declare (type fixnum n))
 (declare (type fixnum m)) (declare (type double-float cond))
 (declare (type double-float unitrd)) (declare (type double-float averr))
 (prog ((rmin 0.0d0) (float2 0.0d0) (float1 0.0d0))
  (declare (type double-float rmin)) (declare (type double-float float2))
  (declare (type double-float float1)) (setf float1 m)
  (setf float2 (/ (float m) (float n)))
  (setf rmin
   (amax1 (* (* (expt float1 1.5) cond) unitrd) (* (expt float2 1.5) averr))
  )
  (return rmin)
))

