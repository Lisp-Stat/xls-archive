(defun abslv (mo m n a na b nb c nc wk ierr) (declare (type fixnum mo))
 (declare (type fixnum m)) (declare (type fixnum n))
 (declare (type (simple-array float (* *)) a)) (declare (type fixnum na))
 (declare (type (simple-array float (* *)) b)) (declare (type fixnum nb))
 (declare (type (simple-array float (* *)) c)) (declare (type fixnum nc))
 (declare (type (simple-array float (*)) wk)) (declare (type fixnum ierr))
 (prog ((iw 0) (iv 0) (iu 0)) (declare (type fixnum iw))
  (declare (type fixnum iv)) (declare (type fixnum iu)) (setf iu 1)
  (setf iv (+ (* m m) 1)) (setf iw (+ (* n n) iv))
  (multiple-value-setq
   (mo m n a na dummy_var m b nb dummy_var n c nc dummy_var ierr)
   (abslv1 mo m n a na (fref wk iu) m b nb (fref wk iv) n c nc (fref wk iw)
    ierr
  ))
  (return (values mo m n a na b nb c nc wk ierr))
))

