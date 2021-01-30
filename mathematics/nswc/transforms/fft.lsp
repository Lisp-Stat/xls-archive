(defun fft (c n isn ierr) (declare (type (simple-array float (*)) c))
 (declare (type fixnum n)) (declare (type fixnum isn))
 (declare (type fixnum ierr))
 (prog nil (if (/= (iabs isn) 1) (go label10))
  (multiple-value-setq (dummy_var dummy_var n n n dummy_var ierr)
   (sfft (fref c 1) (fref c 2) n n n (+ isn isn) ierr)
  )
  (go end_label) label10 (setf ierr 4) (go end_label) end_label
  (return (values c n isn ierr))
))

