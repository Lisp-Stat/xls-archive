(defun
 (comment
  "     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1973 june 15"
 )
 (comment
  "     to appear in @solving least squares problems@, prentice-hall, 1974"
 )
 (declare (type double-float comment))
 (declare
  (type double-float
   "     to appear in @solving least squares problems@, prentice-hall, 1974"
 ))
 (prog
  ((index (make-array '(1) :element-type 'fixnum))
   (a (make-array '(mda n) :element-type 'double-float))
   (b (make-array '(1) :element-type 'double-float))
   (x (make-array '(1) :element-type 'double-float))
   (w (make-array '(1) :element-type 'double-float))
   (zz (make-array '(1) :element-type 'double-float)) (rnorm 0.0d0) (ss 0.0d0)
   (cc 0.0d0) (ii 0) (t_ 0.0d0) (ip 0) (alpha 0.0d0) (jj 0) (jz 0)
   (ztest 0.0d0) (unorm 0.0d0) (asave 0.0d0) (izmax 0) (wmax 0.0d0) (l 0)
   (sm 0.0d0) (j 0) (iz 0) (npp1 0) (nsetp 0) (iz1 0) (iz2 0) (i 0) (itmax 0)
   (iter 0) (n 0) (m 0) (mode 0) (factor 0.0d0) (two 0.0d0) (one 0.0d0)
   (zero 0.0d0)
  )
  (declare (type (simple-array fixnum (*)) index))
  (declare (type (simple-array double-float (* *)) a))
  (declare (type (simple-array double-float (*)) b))
  (declare (type (simple-array double-float (*)) x))
  (declare (type (simple-array double-float (*)) w))
  (declare (type (simple-array double-float (*)) zz))
  (declare (type double-float rnorm)) (declare (type double-float ss))
  (declare (type double-float cc)) (declare (type fixnum ii))
  (declare (type double-float t_)) (declare (type fixnum ip))
  (declare (type double-float alpha)) (declare (type fixnum jj))
  (declare (type fixnum jz)) (declare (type double-float ztest))
  (declare (type double-float unorm)) (declare (type double-float asave))
  (declare (type fixnum izmax)) (declare (type double-float wmax))
  (declare (type fixnum l)) (declare (type double-float sm))
  (declare (type fixnum j)) (declare (type fixnum iz))
  (declare (type fixnum npp1)) (declare (type fixnum nsetp))
  (declare (type fixnum iz1)) (declare (type fixnum iz2))
  (declare (type fixnum i)) (declare (type fixnum itmax))
  (declare (type fixnum iter)) (declare (type fixnum n))
  (declare (type fixnum m)) (declare (type fixnum mode))
  (declare (type double-float factor)) (declare (type double-float two))
  (declare (type double-float one)) (declare (type double-float zero))
  (comment "")
  (comment "         **********   nonnegative least squares   **********")
  (comment "")
  (comment "     given an m by n matrix, a, and an m-vector, b,  compute an")
  (comment "     n-vector, x, which solves the least squares problem")
  (comment "") (comment "                      a * x = b  subject to x .ge. 0")
  (comment "")
  (comment
   "     a(),mda,m,n     mda is the first dimensioning parameter for the"
  )
  (comment
   "                     array, a().   on entry a() contains the m by n"
  )
  (comment "                     matrix, a.           on exit a() contains")
  (comment "                     the product matrix, q*a , where q is an")
  (comment
   "                     m by m orthogonal matrix generated implicitly by"
  )
  (comment "                     this subroutine.")
  (comment
   "     b()     on entry b() contains the m-vector, b.   on exit b() con-"
  )
  (comment "             tains q*b.")
  (comment
   "     x()     on entry x() need not be initialized.  on exit x() will"
  )
  (comment "             contain the solution vector.")
  (comment "     rnorm   on exit rnorm contains the euclidean norm of the")
  (comment "             residual vector.")
  (comment
   "     w()     an n-array of working space.  on exit w() will contain"
  )
  (comment "             the dual solution vector.   w will satisfy w(i) = 0.")
  (comment
   "             for all i in set p  and w(i) .le. 0. for all i in set z"
  )
  (comment "     zz()     an m-array of working space.")
  (comment "     index()     an integer working array of length at least n.")
  (comment
   "                 on exit the contents of this array define the sets"
  )
  (comment "                 p and z as follows..") (comment "")
  (comment "                 index(1)   thru index(nsetp) = set p.")
  (comment "                 index(iz1) thru index(iz2)   = set z.")
  (comment "                 iz1 = nsetp + 1 = npp1")
  (comment "                 iz2 = n")
  (comment "     mode    this is a success-failure flag with the following")
  (comment "             meanings.")
  (comment "             1     the solution has been computed successfully.")
  (comment "             2     the dimensions of the problem are bad.")
  (comment "                   either m .le. 0 or n .le. 0.")
  (comment
   "             3    iteration count exceeded.  more than 3*n iterations."
  )
  (comment "") defun nnls (a mda m n b x rnorm w zz index mode) (setf zero 0.0)
  (setf one 1.0) (setf two 2.0) (setf factor 0.01) (comment "") (setf mode 1)
  (if (and (> m 0) (> n 0)) (go label10)) (setf mode 2) (go end_label) label10
  (setf iter 0) (setf itmax (* 3 n)) (comment "")
  (comment "                    initialize the arrays index() and x().")
  (comment "")
  (fdo ((i 1 (+ i 1))) ((> i n) nil)
   (tagbody (fset (fref x i) zero) (fset (index i) i))
  )
  (comment "") (setf iz2 n) (setf iz1 1) (setf nsetp 0) (setf npp1 1)
  (comment "                             ******  main loop begins here  ******"
  )
  label30
  (comment
   "                  quit if all coefficients are already in the solution."
  )
  (comment
   "                        or if m cols of a have been triangularized."
  )
  (comment "") (if (or (> iz1 iz2) (>= nsetp m)) (go label350)) (comment "")
  (comment
   "         compute components of the dual (negative gradient) vector w()."
  )
  (comment "")
  (fdo ((iz iz1 (+ iz 1))) ((> iz iz2) nil)
   (tagbody (setf j (index iz)) (setf sm zero)
    (fdo ((l npp1 (+ l 1))) ((> l m) nil)
     (tagbody (setf sm (+ sm (* (fref a l j) (fref b l)))))
    )
    (fset (fref w j) sm)
  ))
  (comment "                                   find largest positive w(j).")
  label60 (setf wmax zero)
  (fdo ((iz iz1 (+ iz 1))) ((> iz iz2) nil)
   (tagbody (setf j (index iz)) (if (<= (fref w j) wmax) (go label70))
    (setf wmax (fref w j)) (setf izmax iz) label70
  ))
  (comment "") (comment "             if wmax .le. 0. go to termination.")
  (comment
   "             this indicates satisfaction of the kuhn-tucker conditions."
  )
  (comment "") (arithmetic-if wmax (go label350) (go label350) (go label80))
  label80 (setf iz izmax) (setf j (index iz)) (comment "")
  (comment "     the sign of w(j) is ok for j to be moved to set p.")
  (comment
   "     begin the transformation and check new diagonal element to avoid"
  )
  (comment "     near linear dependence.") (comment "")
  (setf asave (fref a npp1 j))
  (multiple-value-setq
   (dummy_var npp1 dummy_var m dummy_var dummy_var up dummy dummy_var dummy_var
    dummy_var
   )
   (h12 1 npp1 (+ npp1 1) m (fref a 1 j) 1 up dummy 1 1 0)
  )
  (setf unorm zero) (if (= nsetp 0) (go label100))
  (fdo ((l 1 (+ l 1))) ((> l nsetp) nil)
   (tagbody (setf unorm (+ unorm (expt (fref a l j) 2))))
  )
  label100 (setf unorm (sqrt unorm))
  (arithmetic-if (diff (+ unorm (* (abs (fref a npp1 j)) factor)) unorm)
   (go label130) (go label130) (go label110)
  )
  (comment "")
  (comment
   "     col j is sufficiently independent.  copy b into zz, update zz and"
  )
  (comment "   > solve for ztest ( = proposed new value for x(j) ).")
  (comment "") label110
  (fdo ((l 1 (+ l 1))) ((> l m) nil) (tagbody (fset (fref zz l) (fref b l))))
  (multiple-value-setq
   (dummy_var npp1 dummy_var m dummy_var dummy_var up zz dummy_var dummy_var
    dummy_var
   )
   (h12 2 npp1 (+ npp1 1) m (fref a 1 j) 1 up zz 1 1 1)
  )
  (setf ztest (/ (fref zz npp1) (fref a npp1 j))) (comment "")
  (comment "                                     see if ztest is positive")
  (comment "     reject j as a candidate to be moved from set z to set p.")
  (comment "     restore a(npp1,j), set w(j)=0., and loop back to test dual")
  (comment "") (arithmetic-if ztest (go label130) (go label130) (go label140))
  (comment "") (comment "     coeffs again.") (comment "") label130
  (fset (fref a npp1 j) asave) (fset (fref w j) zero) (go label60) (comment "")
  (comment "     the index  j=index(iz)  has been selected to be moved from")
  (comment
   "     set z to set p.    update b,  update indices,  apply householder"
  )
  (comment
   "     transformations to cols in new set z,  zero subdiagonal elts in"
  )
  (comment "     col j,  set w(j)=0.") (comment "") label140
  (fdo ((l 1 (+ l 1))) ((> l m) nil) (tagbody (fset (fref b l) (fref zz l))))
  (comment "") (fset (index iz) (index iz1)) (fset (index iz1) j)
  (setf iz1 (+ iz1 1)) (setf nsetp npp1) (setf npp1 (+ npp1 1)) (comment "")
  (if (> iz1 iz2) (go label170))
  (fdo ((jz iz1 (+ jz 1))) ((> jz iz2) nil)
   (tagbody (setf jj (index jz))
    (multiple-value-setq
     (dummy_var nsetp npp1 m dummy_var dummy_var up dummy_var dummy_var mda
      dummy_var
     )
     (h12 2 nsetp npp1 m (fref a 1 j) 1 up (fref a 1 jj) 1 mda 1)
  )))
  label170 (comment "") (if (= nsetp m) (go label190))
  (fdo ((l npp1 (+ l 1))) ((> l m) nil) (tagbody (fset (fref a l j) zero)))
  label190 (comment "") (fset (fref w j) zero)
  (comment "                                solve the triangular system.")
  (comment
   "                                store the solution temporarily in zz()."
  )
  ----> (assign 200 to next) (go label400) (comment "")
  (comment "                       ******  secondary loop begins here ******")
  (comment "") (comment "                          iteration counter.")
  (comment "") label210 (setf iter (+ iter 1))
  (if (<= iter itmax) (go label220)) (setf mode 3)
  (format 6
   "~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A"
   (list)
  )
  (go label350) label220 (comment "")
  (comment
   "                    see if all new constrained coeffs are feasible."
  )
  (comment "                                  if not compute alpha.")
  (comment "") (setf alpha two)
  (fdo ((ip 1 (+ ip 1))) ((> ip nsetp) nil)
   (tagbody (setf l (index ip))
    (arithmetic-if (fref zz ip) (go label230) (go label230) (go label240))
    (comment "") label230
    (setf t_ (/ (* -1 (fref x l)) (+ (fref zz ip) (- (fref x l)))))
    (if (<= alpha t_) (go label240)) (setf alpha t_) (setf jj ip) label240
  ))
  (comment "")
  (comment
   "          if all new constrained coeffs are feasible then alpha will"
  )
  (comment
   "          still = 2.    if so exit from secondary loop to main loop."
  )
  (comment "") (if (= alpha two) (go label330)) (comment "")
  (comment "          otherwise use alpha which will be between 0. and 1. to")
  (comment "          interpolate between the old x and the new zz.")
  (comment "")
  (fdo ((ip 1 (+ ip 1))) ((> ip nsetp) nil)
   (tagbody (setf l (index ip))
    (fset (fref x l) (+ (fref x l) (* alpha (+ (fref zz ip) (- (fref x l))))))
  ))
  (comment "")
  (comment "        modify a and b and the index arrays to move coefficient i")
  (comment "        from set p to set z.") (comment "") (setf i (index jj))
  label260 (fset (fref x i) zero) (comment "") (if (= jj nsetp) (go label290))
  (setf jj (+ jj 1))
  (fdo ((j jj (+ j 1))) ((> j nsetp) nil)
   (tagbody (setf ii (index j)) (fset (index (+ j (- 1))) ii)
    (multiple-value-setq (dummy_var dummy_var cc ss dummy_var)
     (g1 (fref a (+ j (- 1)) ii) (fref a j ii) cc ss (fref a (+ j (- 1)) ii))
    )
    (fset (fref a j ii) zero)
    (fdo ((l 1 (+ l 1))) ((> l n) nil)
     (tagbody
      (if (/= l ii)
       (multiple-value-setq (cc ss dummy_var dummy_var)
        (g2 cc ss (fref a (+ j (- 1)) l) (fref a j l))
    ))))
    (multiple-value-setq (cc ss dummy_var dummy_var)
     (g2 cc ss (fref b (+ j (- 1))) (fref b j))
  )))
  label290 (setf npp1 nsetp) (setf nsetp (+ nsetp (- 1)))
  (setf iz1 (+ iz1 (- 1))) (fset (index iz1) i) (comment "")
  (comment
   "        see if the remaining coeffs in set p are feasible.  they should"
  )
  (comment "        be because of the way alpha was determined.")
  (comment "        if any are infeasible it is due to round-off error.  any")
  (comment "        that are nonpositive will be set to zero")
  (comment "        and moved from set p to set z.") (comment "")
  (fdo ((jj 1 (+ jj 1))) ((> jj nsetp) nil)
   (tagbody (setf i (index jj))
    (arithmetic-if (fref x i) (go label260) (go label260) (go label300))
    label300
  ))
  (comment "")
  (comment "         copy b( ) into zz( ).  then solve again and loop back.")
  (comment "")
  (fdo ((i 1 (+ i 1))) ((> i m) nil) (tagbody (fset (fref zz i) (fref b i))))
  ----> (assign 320 to next) (go label400) (go label210)
  (comment "                      ******  end of secondary loop  ******")
  (comment "") label330
  (fdo ((ip 1 (+ ip 1))) ((> ip nsetp) nil)
   (tagbody (setf i (index ip)) (fset (fref x i) (fref zz ip)))
  )
  (comment "        all new coeffs are positive.  loop back to beginning.")
  (go label30) (comment "")
  (comment "                        ******  end of main loop  ******")
  (comment "")
  (comment "                        come to here for termination.")
  (comment
   "                     compute the norm of the final residual vector."
  )
  (comment "") label350 (setf sm zero) (if (> npp1 m) (go label370))
  (fdo ((i npp1 (+ i 1))) ((> i m) nil)
   (tagbody (setf sm (+ sm (expt (fref b i) 2))))
  )
  (go label390) label370
  (fdo ((j 1 (+ j 1))) ((> j n) nil) (tagbody (fset (fref w j) zero))) label390
  (setf rnorm (sqrt sm)) (go end_label) (comment "")
  (comment "     the following block of code is used as an internal subroutine"
  )
  (comment "     to solve the triangular system, putting the solution in zz()."
  )
  (comment "") label400
  (fdo ((l 1 (+ l 1))) ((> l nsetp) nil)
   (tagbody (setf ip (+ (+ nsetp 1) (- l))) (if (= l 1) (go label420))
    (fdo ((ii 1 (+ ii 1))) ((> ii ip) nil)
     (tagbody
      (fset (fref zz ii)
       (+ (fref zz ii) (* (* -1 (fref a ii jj)) (fref zz (+ ip 1))))
    )))
    label420 (setf jj (index ip))
    (fset (fref zz ip) (/ (fref zz ip) (fref a ip jj)))
  ))
  (computed-goto next 'nil) end_label
  (return
   (values comment
    "     to appear in @solving least squares problems@, prentice-hall, 1974"
))))

