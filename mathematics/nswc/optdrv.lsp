(defun optdrv
 (mo nr n x fcn typsiz fscale rerr itnlim itncnt gradtl stepmx steptl xpls fpls
  gpls ierr a g p wrk0 wrk1 wrk2
 )
 (declare (type fixnum mo)) (declare (type fixnum nr))
 (declare (type fixnum n)) (declare (type (simple-array double-float (*)) x))
 (declare (type (simple-array double-float (*)) typsiz))
 (declare (type double-float fscale)) (declare (type double-float rerr))
 (declare (type fixnum itnlim)) (declare (type fixnum itncnt))
 (declare (type double-float gradtl)) (declare (type double-float stepmx))
 (declare (type double-float steptl))
 (declare (type (simple-array double-float (*)) xpls))
 (declare (type double-float fpls))
 (declare (type (simple-array double-float (*)) gpls))
 (declare (type fixnum ierr))
 (declare (type (simple-array double-float (* *)) a))
 (declare (type (simple-array double-float (*)) g))
 (declare (type (simple-array double-float (*)) p))
 (declare (type (simple-array double-float (*)) wrk0))
 (declare (type (simple-array double-float (*)) wrk1))
 (declare (type (simple-array double-float (*)) wrk2))
 (prog
  ((mxtake nil) (noupdt nil) (sptr 0)
   (fstack (make-array '(30) :element-type 'double-float)) (f 0.0d0)
   (cbrnf 0.0d0) (i 0) (jp1 0) (j 0) (nm1 0) (icscmx 0) (iretcd 0) (iagflg 0)
   (sqrnf 0.0d0) (rnf 0.0d0) (stmx 0.0d0) (ns 0)
  )
  (declare (type t mxtake)) (declare (type t noupdt))
  (declare (type fixnum sptr))
  (declare (type (simple-array double-float (*)) fstack))
  (declare (type double-float f)) (declare (type double-float cbrnf))
  (declare (type fixnum i)) (declare (type fixnum jp1))
  (declare (type fixnum j)) (declare (type fixnum nm1))
  (declare (type fixnum icscmx)) (declare (type fixnum iretcd))
  (declare (type fixnum iagflg)) (declare (type double-float sqrnf))
  (declare (type double-float rnf)) (declare (type double-float stmx))
  (declare (type fixnum ns)) (setf ns 30) (setf stmx stepmx) (setf stepmx 0.0)
  (multiple-value-setq (n x typsiz fscale gradtl itnlim rerr stepmx ierr)
   (opchk1 n x typsiz fscale gradtl itnlim rerr stepmx ierr)
  )
  (if (< ierr 0) (go end_label))
  (if (/= mo 0) (setf stepmx (amax1 stmx stepmx)))
  (setf rnf (* 2.0 (amax1 rerr (spmpar 1)))) (setf sqrnf (sqrt rnf))
  (setf itncnt 0) (setf iagflg 0) (setf iretcd (- 1)) (setf icscmx 0)
  (multiple-value-setq (n x f) (fcn n x f))
  (multiple-value-setq (n x fcn f g typsiz sqrnf)
   (fstofd n x fcn f g typsiz sqrnf)
  )
  (multiple-value-setq
   (n x f g wrk1 itncnt icscmx ierr gradtl steptl typsiz fscale itnlim iretcd
    mxtake fstack ns sptr
   )
   (opstp n x f g wrk1 itncnt icscmx ierr gradtl steptl typsiz fscale itnlim
    iretcd mxtake fstack ns sptr
  ))
  (if (= ierr 0) (go label10)) (if (/= mo 0) (go label210))
  (multiple-value-setq (fcn n x f dummy_var) (fxdec fcn n x f 10.0))
  (setf stepmx 0.0)
  (multiple-value-setq (n x typsiz fscale gradtl itnlim rerr stepmx ierr)
   (opchk1 n x typsiz fscale gradtl itnlim rerr stepmx ierr)
  )
  (multiple-value-setq (n x fcn f g typsiz sqrnf)
   (fstofd n x fcn f g typsiz sqrnf)
  )
  (multiple-value-setq
   (n x f g wrk1 itncnt icscmx ierr gradtl steptl typsiz fscale itnlim iretcd
    mxtake fstack ns sptr
   )
   (opstp n x f g wrk1 itncnt icscmx ierr gradtl steptl typsiz fscale itnlim
    iretcd mxtake fstack ns sptr
  ))
  (if (/= ierr 0) (go label210)) label10 (setf nm1 (+ n (- 1)))
  (fdo ((j 1 (+ j 1))) ((> j nm1) nil)
   (tagbody (fset (fref a j j) (/ 1.0 (fref typsiz j))) (setf jp1 (+ j 1))
    (fdo ((i jp1 (+ i 1))) ((> i n) nil) (tagbody (fset (fref a i j) 0.0)))
  ))
  (fset (fref a n n) (/ 1.0 (fref typsiz n))) (go label101) label100
  (if (> mo 1) (go label101)) (if (/= (mod itncnt 10) 0) (go label101))
  (if (>= (+ itncnt 10) itnlim) (go label101))
  (multiple-value-setq (mo x typsiz n ierr) (scalex mo x typsiz n ierr))
  (if (= ierr 0) (go label101)) (setf mo (+ mo 1)) (go end_label) label101
  (setf itncnt (+ itncnt 1)) label105
  (fdo ((i 1 (+ i 1))) ((> i n) nil)
   (tagbody (fset (fref wrk1 i) (- (fref g i))))
  )
  (multiple-value-setq (nr n a p wrk1) (lltslv nr n a p wrk1))
  (multiple-value-setq
   (n x f g p xpls fpls fcn mxtake iretcd stepmx steptl typsiz)
   (lnsrch n x f g p xpls fpls fcn mxtake iretcd stepmx steptl typsiz)
  )
  (if (or (/= iretcd 1) (/= iagflg 0)) (go label120)) (setf iagflg (- 1))
  (setf cbrnf (expt rnf (/ 1.0 3.0)))
  (multiple-value-setq (n x fcn typsiz cbrnf g) (fstocd n x fcn typsiz cbrnf g)
  )
  (go label105) label120 (if (= iagflg 0) (go label130))
  (multiple-value-setq (n xpls fcn typsiz cbrnf gpls)
   (fstocd n xpls fcn typsiz cbrnf gpls)
  )
  (go label140) label130
  (multiple-value-setq (n xpls fcn fpls gpls typsiz sqrnf)
   (fstofd n xpls fcn fpls gpls typsiz sqrnf)
  )
  label140
  (multiple-value-setq
   (n xpls fpls gpls x itncnt icscmx ierr gradtl steptl typsiz fscale itnlim
    iretcd mxtake fstack ns sptr
   )
   (opstp n xpls fpls gpls x itncnt icscmx ierr gradtl steptl typsiz fscale
    itnlim iretcd mxtake fstack ns sptr
  ))
  (if (/= ierr 0) (go label200))
  (multiple-value-setq
   (nr n x g a xpls gpls itncnt sqrnf noupdt wrk0 wrk1 wrk2)
   (secfac nr n x g a xpls gpls itncnt sqrnf noupdt wrk0 wrk1 wrk2)
  )
  (setf f fpls)
  (fdo ((i 1 (+ i 1))) ((> i n) nil)
   (tagbody (fset (fref x i) (fref xpls i)) (fset (fref g i) (fref gpls i)))
  )
  (go label100) label200 (if (/= ierr 3) (go end_label)) label210 (setf fpls f)
  (fdo ((i 1 (+ i 1))) ((> i n) nil)
   (tagbody (fset (fref xpls i) (fref x i)) (fset (fref gpls i) (fref g i)))
  )
  (go end_label) end_label
  (return
   (values mo nr n x fcn typsiz fscale rerr itnlim itncnt gradtl stepmx steptl
    xpls fpls gpls ierr a g p wrk0 wrk1 wrk2
))))

