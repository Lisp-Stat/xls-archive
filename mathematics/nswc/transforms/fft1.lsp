(defun fft1 (a b n isn ierr) (declare (type (simple-array float (*)) a))
 (declare (type (simple-array float (*)) b)) (declare (type fixnum n))
 (declare (type fixnum isn)) (declare (type fixnum ierr))
 (prog nil (if (/= (iabs isn) 1) (go label10))
  (multiple-value-setq (a b n n n isn ierr) (sfft a b n n n isn ierr))
  (go end_label) label10 (setf ierr 4) (go end_label) end_label
  (return (values a b n isn ierr))
))

