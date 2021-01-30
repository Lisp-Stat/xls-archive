(defun cheby (a b f g phi eps itno mxiter l m p q error ierr w)
 (declare (type double-float a)) (declare (type double-float b))
 (declare (type double-float eps)) (declare (type fixnum itno))
 (declare (type fixnum mxiter)) (declare (type fixnum l))
 (declare (type fixnum m)) (declare (type (simple-array double-float (*)) p))
 (declare (type (simple-array double-float (*)) q))
 (declare (type double-float error)) (declare (type fixnum ierr))
 (declare (type (simple-array double-float (*)) w))
 (prog
  ((f 0.0d0) (g 0.0d0) (phi 0.0d0) (i5 0) (i4 0) (i3 0) (i2 0) (i1 0) (np1 0)
   (n 0) (lpm 0) (mp1 0) (lp1 0)
  )
  (declare (type double-float f)) (declare (type double-float g))
  (declare (type double-float phi)) (declare (type fixnum i5))
  (declare (type fixnum i4)) (declare (type fixnum i3))
  (declare (type fixnum i2)) (declare (type fixnum i1))
  (declare (type fixnum np1)) (declare (type fixnum n))
  (declare (type fixnum lpm)) (declare (type fixnum mp1))
  (declare (type fixnum lp1)) (if (or (< l 0) (< m 0)) (go label10))
  (setf lp1 (+ l 1)) (setf mp1 (+ m 1)) (setf lpm (+ l m)) (setf n (+ lpm 1))
  (setf np1 (+ n 1)) (setf i1 (+ np1 1)) (setf i2 (+ i1 np1))
  (setf i3 (+ i2 (* np1 np1))) (setf i4 (+ i3 np1)) (setf i5 (+ i4 np1))
  (multiple-value-setq
   (a b f g phi eps itno mxiter l m p q error ierr lp1 mp1 lpm n np1 dummy_var
    dummy_var dummy_var dummy_var dummy_var dummy_var
   )
   (cheby1 a b f g phi eps itno mxiter l m p q error ierr lp1 mp1 lpm n np1
    (fref w 1) (fref w i1) (fref w i2) (fref w i3) (fref w i4) (fref w i5)
  ))
  (go end_label) label10 (setf ierr 1) (go end_label) end_label
  (return (values a b f g phi eps itno mxiter l m p q error ierr w))
))

