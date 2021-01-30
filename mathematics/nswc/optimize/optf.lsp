(defun optf (fcn n rerr iter xpls fpls ierr wrk) (declare (type fixnum n))
 (declare (type double-float rerr)) (declare (type fixnum iter))
 (declare (type (simple-array float (*)) xpls))
 (declare (type double-float fpls)) (declare (type fixnum ierr))
 (declare (type (simple-array float (* *)) wrk))
 (prog
  ((fscale 0.0d0) (i 0) (itnlim 0) (mo 0) (steptl 0.0d0) (stepmx 0.0d0)
   (gradtl 0.0d0) (eps 0.0d0)
  )
  (declare (type double-float fscale)) (declare (type fixnum i))
  (declare (type fixnum itnlim)) (declare (type fixnum mo))
  (declare (type double-float steptl)) (declare (type double-float stepmx))
  (declare (type double-float gradtl)) (declare (type double-float eps))
  (setf eps (amax1 (spmpar 1) (abs rerr))) (setf gradtl (expt eps 0.4))
  (setf stepmx 0.0) (setf steptl eps)
  (if (<= eps 1.0E-10) (setf steptl (* 10.0 eps)))
  (if (< eps 1.0E-13) (setf steptl (* 100.0 eps))) (setf mo 0)
  (setf itnlim iter) (setf iter 0)
  (fdo ((i 1 (+ i 1))) ((> i n) nil)
   (tagbody (fset (fref wrk i 1) (fref xpls i)) (fset (fref wrk i 2) 1.0))
  )
  (setf fscale 1.0) label20
  (multiple-value-setq
   (mo n n dummy_var fcn dummy_var fscale rerr itnlim itncnt gradtl stepmx
    steptl xpls fpls dummy_var ierr dummy_var dummy_var dummy_var dummy_var
    dummy_var dummy_var
   )
   (optdrv mo n n (fref wrk 1 1) fcn (fref wrk 1 2) fscale rerr itnlim itncnt
    gradtl stepmx steptl xpls fpls (fref wrk 1 3) ierr (fref wrk 1 9)
    (fref wrk 1 4) (fref wrk 1 5) (fref wrk 1 6) (fref wrk 1 7) (fref wrk 1 8)
  ))
  (setf iter (+ iter itncnt)) (if (/= ierr (- 10)) (go end_label))
  (setf itnlim (+ itnlim (- itncnt)))
  (return (values fcn n rerr iter xpls fpls ierr wrk))
))

