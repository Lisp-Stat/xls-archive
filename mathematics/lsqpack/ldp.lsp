(defun ldp (g mdg m n h x xnorm w index mode)
 (declare (type (simple-array double-float (* *)) g))
 (declare (type fixnum mdg)) (declare (type fixnum m))
 (declare (type fixnum n)) (declare (type (simple-array double-float (*)) h))
 (declare (type (simple-array double-float (*)) x))
 (declare (type double-float xnorm))
 (declare (type (simple-array double-float (*)) w))
 (declare (type (simple-array fixnum (*)) index)) (declare (type fixnum mode))
 (prog
  ((fac 0.0d0) (iwdual 0) (iy 0) (iz 0) (np1 0) (ik 0) (i 0) (iw 0) (j 0)
   (one 0.0d0) (zero 0.0d0)
  )
  (declare (type double-float fac)) (declare (type fixnum iwdual))
  (declare (type fixnum iy)) (declare (type fixnum iz))
  (declare (type fixnum np1)) (declare (type fixnum ik))
  (declare (type fixnum i)) (declare (type fixnum iw))
  (declare (type fixnum j)) (declare (type double-float one))
  (declare (type double-float zero))
  (comment
   "     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1974 mar 1"
  )
  (comment
   "     to appear in @solving least squares problems@, prentice-hall, 1974"
  )
  (comment "")
  (comment "            **********  least distance programming  **********")
  (comment "") (setf zero 0.0) (setf one 1.0) (if (<= n 0) (go label120))
  (fdo ((j 1 (+ j 1))) ((> j n) nil) (tagbody (fset (fref x j) zero)))
  (setf xnorm zero) (if (<= m 0) (go label110)) (comment "")
  (comment
   "     the declared dimension of w() must be at least (n+1)*(m+2)+2*m."
  )
  (comment "")
  (comment "      first (n+1)*m locs of w()   =  matrix e for problem nnls.")
  (comment "       next     n+1 locs of w()   =  vector f for problem nnls.")
  (comment "       next     n+1 locs of w()   =  vector z for problem nnls.")
  (comment "       next       m locs of w()   =  vector y for problem nnls.")
  (comment
   "       next       m locs of w()   =  vector wdual for problem nnls."
  )
  (comment "     copy g**t into first n rows and m columns of e.")
  (comment "     copy h**t into row n+1 of e.") (comment "") (setf iw 0)
  (fdo ((j 1 (+ j 1))) ((> j m) nil)
   (tagbody
    (fdo ((i 1 (+ i 1))) ((> i n) nil)
     (tagbody (setf iw (+ iw 1)) (fset (fref w iw) (fref g j i)))
    )
    (setf iw (+ iw 1)) (fset (fref w iw) (fref h j))
  ))
  (setf ik (+ iw 1))
  (comment
   "                                store n zeros followed by a one into f."
  )
  (fdo ((i 1 (+ i 1))) ((> i n) nil)
   (tagbody (setf iw (+ iw 1)) (fset (fref w iw) zero))
  )
  (fset (fref w (+ iw 1)) one) (comment "") (setf np1 (+ n 1))
  (setf iz (+ iw 2)) (setf iy (+ iz np1)) (setf iwdual (+ iy m)) (comment "")
  (multiple-value-setq
   (w np1 np1 m dummy_var dummy_var rnorm dummy_var dummy_var index mode)
   (nnls w np1 np1 m (fref w ik) (fref w iy) rnorm (fref w iwdual) (fref w iz)
    index mode
  ))
  (comment
   "                      use the following return if unsuccessful in nnls."
  )
  (if (/= mode 1) (go end_label))
  (arithmetic-if rnorm (go label130) (go label130) (go label50)) label50
  (setf fac one) (setf iw (+ iy (- 1)))
  (fdo ((i 1 (+ i 1))) ((> i m) nil)
   (tagbody (setf iw (+ iw 1))
    (comment
     "                               here we are using the solution vector y."
    )
    (setf fac (+ fac (* (* -1 (fref h i)) (fref w iw))))
  ))
  (comment "")
  (arithmetic-if (diff (+ one fac) one) (go label130) (go label130)
   (go label70)
  )
  label70 (setf fac (/ one fac))
  (fdo ((j 1 (+ j 1))) ((> j n) nil)
   (tagbody (setf iw (+ iy (- 1)))
    (fdo ((i 1 (+ i 1))) ((> i m) nil)
     (tagbody (setf iw (+ iw 1))
      (comment
       "                               here we are using the solution vector y."
      )
      (fset (fref x j) (+ (fref x j) (* (fref g i j) (fref w iw))))
    ))
    (fset (fref x j) (* (fref x j) fac))
  ))
  (fdo ((j 1 (+ j 1))) ((> j n) nil)
   (tagbody (setf xnorm (+ xnorm (expt (fref x j) 2))))
  )
  (setf xnorm (sqrt xnorm))
  (comment "                             successful return.") label110
  (setf mode 1) (go end_label)
  (comment "                             error return.       n .le. 0.")
  label120 (setf mode 2) (go end_label)
  (comment
   "                             returning with constraints not compatible."
  )
  label130 (setf mode 4) (go end_label) end_label
  (return (values g mdg m n h x xnorm w index mode))
))

