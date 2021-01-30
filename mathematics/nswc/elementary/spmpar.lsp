(defun spmpar (i) (declare (type fixnum i))
 (prog
  ((emin 0) (emax 0) (spmpar 0.0d0) (z 0.0d0) (bm1 0.0d0) (ibeta 0) (w 0.0d0)
   (binv 0.0d0) (one 0.0d0) (m 0) (b 0.0d0)
  )
  (declare (type fixnum emin)) (declare (type fixnum emax))
  (declare (type real spmpar)) (declare (type double-float z))
  (declare (type double-float bm1)) (declare (type fixnum ibeta))
  (declare (type double-float w)) (declare (type double-float binv))
  (declare (type double-float one)) (declare (type fixnum m))
  (declare (type double-float b)) (if (> i 1) (go label10)) (setf b (ipmpar 4))
  (setf m (ipmpar 5)) (setf spmpar (expt b (+ 1 (- m)))) (go end_label) label10
  (if (> i 2) (go label20)) (setf b (ipmpar 4)) (setf emin (ipmpar 6))
  (setf one (float 1)) (setf binv (/ one b)) (setf w (expt b (+ emin 2)))
  (setf spmpar (* (* (* w binv) binv) binv)) (go end_label) label20
  (setf ibeta (ipmpar 4)) (setf m (ipmpar 5)) (setf emax (ipmpar 7))
  (setf b ibeta) (setf bm1 (+ ibeta (- 1))) (setf one (float 1))
  (setf z (expt b (+ m (- 1))))
  (setf w (/ (+ (* (+ z (- one)) b) bm1) (* b z)))
  (setf z (expt b (+ emax (- 2)))) (setf spmpar (* (* (* w z) b) b))
  (go end_label) end_label (return spmpar)
))

