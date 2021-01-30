(defun assgn (n a c t_ iwk ierr) (declare (type fixnum n))
 (declare (type (simple-array fixnum (* *)) a))
 (declare (type (simple-array fixnum (*)) c)) (declare (type fixnum t_))
 (declare (type (simple-array fixnum (*)) iwk)) (declare (type fixnum ierr))
 (prog ((i6 0) (i5 0) (i4 0) (i3 0) (i2 0) (i1 0)) (declare (type fixnum i6))
  (declare (type fixnum i5)) (declare (type fixnum i4))
  (declare (type fixnum i3)) (declare (type fixnum i2))
  (declare (type fixnum i1)) (setf i1 (+ n 1)) (setf i2 (+ i1 n))
  (setf i3 (+ i2 n)) (setf i4 (+ (+ i3 n) 1)) (setf i5 (+ i4 n))
  (setf i6 (+ i5 n))
  (multiple-value-setq
   (n a c t_ dummy_var dummy_var dummy_var dummy_var dummy_var dummy_var
    dummy_var dummy_var dummy_var ierr
   )
   (assgn1 n a c t_ (fref iwk 1) (fref iwk i1) (fref iwk i2) (fref iwk i3)
    (fref iwk 1) (fref iwk i3) (fref iwk i4) (fref iwk i5) (fref iwk i6) ierr
  ))
  (return (values n a c t iwk ierr))
))

