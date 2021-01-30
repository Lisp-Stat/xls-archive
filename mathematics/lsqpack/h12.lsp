(defun
 (comment
  "     c.l.lawson and r.j.hanson, jet propulsion laboratory, 1973 jun 12"
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
  ((sm 0.0d0) (b 0.0d0) (u (make-array '(iue 1) :element-type 'double-float))
   (c (make-array '(1) :element-type 'double-float)) (i 0) (i4 0) (i3 0)
   (incr 0) (ice 0) (icv 0) (i2 0) (ncv 0) (up 0.0d0) (sm1 0.0d0) (clinv 0.0d0)
   (j 0) (mode 0) (cl 0.0d0) (l1 0) (lpivot 0) (one 0.0d0)
  )
  (declare (type double-float sm)) (declare (type double-float b))
  (declare (type (simple-array double-float (* *)) u))
  (declare (type (simple-array double-float (*)) c)) (declare (type fixnum i))
  (declare (type fixnum i4)) (declare (type fixnum i3))
  (declare (type fixnum incr)) (declare (type fixnum ice))
  (declare (type fixnum icv)) (declare (type fixnum i2))
  (declare (type fixnum ncv)) (declare (type double-float up))
  (declare (type double-float sm1)) (declare (type double-float clinv))
  (declare (type fixnum j)) (declare (type fixnum mode))
  (declare (type double-float cl)) (declare (type fixnum l1))
  (declare (type fixnum lpivot)) (declare (type double-float one)) (comment "")
  (comment "     construction and/or application of a single")
  (comment "     householder transformation..     q = i + u*(u**t)/b")
  (comment "")
  (comment "     mode    = 1 or 2   to select algorithm  h1  or  h2 .")
  (comment "     lpivot is the index of the pivot element.")
  (comment
   "     l1,m   if l1 .le. m   the transformation will be constructed to"
  )
  (comment "            zero elements indexed from l1 through m.   if l1 gt. m"
  )
  (comment "            the subroutine does an identity transformation.")
  (comment "     u(),iue,up    on entry to h1 u() contains the pivot vector.")
  (comment "                   iue is the storage increment between elements.")
  (comment "                                       on exit from h1 u() and up")
  (comment "                   contain quantities defining the vector u of the"
  )
  (comment
   "                   householder transformation.   on entry to h2 u()"
  )
  (comment
   "                   and up should contain quantities previously computed"
  )
  (comment "                   by h1.  these will not be modified by h2.")
  (comment
   "     c()    on entry to h1 or h2 c() contains a matrix which will be"
  )
  (comment "            regarded as a set of vectors to which the householder")
  (comment
   "            transformation is to be applied.  on exit c() contains the"
  )
  (comment "            set of transformed vectors.")
  (comment "     ice    storage increment between elements of vectors in c().")
  (comment "     icv    storage increment between vectors in c().")
  (comment
   "     ncv    number of vectors in c() to be transformed. if ncv .le. 0"
  )
  (comment "            no operations will be done on c().") (comment "") defun
  h12 (mode lpivot l1 m u iue up c ice icv ncv) (setf one 1.0) (comment "")
  (if (or (>= 0 lpivot) (>= lpivot l1) (> l1 m)) (go end_label))
  (setf cl (abs (fref u 1 lpivot))) (if (= mode 2) (go label60))
  (comment
   "                            ****** construct the transformation. ******"
  )
  (fdo ((j l1 (+ j 1))) ((> j m) nil)
   (tagbody (setf cl (amax1 (abs (fref u 1 j)) cl)))
  )
  (arithmetic-if cl (go label130) (go label130) (go label20)) label20
  (setf clinv (/ one cl)) (setf sm (expt (* (dble (fref u 1 lpivot)) clinv) 2))
  (fdo ((j l1 (+ j 1))) ((> j m) nil)
   (tagbody (setf sm (+ sm (expt (* (dble (fref u 1 j)) clinv) 2))))
  )
  (comment
   "                              convert dble. prec. sm to sngl. prec. sm1"
  )
  (setf sm1 sm) (setf cl (* cl (sqrt sm1)))
  (arithmetic-if (fref u 1 lpivot) (go label50) (go label50) (go label40))
  label40 (setf cl (- cl)) label50 (setf up (+ (fref u 1 lpivot) (- cl)))
  (fset (fref u 1 lpivot) cl) (go label70)
  (comment
   "            ****** apply the transformation  i+u*(u**t)/b  to c. ******"
  )
  (comment "") label60
  (arithmetic-if cl (go label130) (go label130) (go label70)) label70
  (if (<= ncv 0) (go end_label)) (setf b (* (dble up) (fref u 1 lpivot)))
  (comment
   "                       b  must be nonpositive here.  if b = 0., return."
  )
  (comment "") (arithmetic-if b (go label80) (go label130) (go label130))
  label80 (setf b (/ one b))
  (setf i2 (+ (+ 1 (- icv)) (* ice (+ lpivot (- 1)))))
  (setf incr (* ice (+ l1 (- lpivot))))
  (fdo ((j 1 (+ j 1))) ((> j ncv) nil)
   (tagbody (setf i2 (+ i2 icv)) (setf i3 (+ i2 incr)) (setf i4 i3)
    (setf sm (* (fref c i2) (dble up)))
    (fdo ((i l1 (+ i 1))) ((> i m) nil)
     (tagbody (setf sm (+ sm (* (fref c i3) (dble (fref u 1 i)))))
      (setf i3 (+ i3 ice))
    ))
    (arithmetic-if sm (go label100) (go label120) (go label100)) label100
    (setf sm (* sm b)) (fset (fref c i2) (+ (fref c i2) (* sm (dble up))))
    (fdo ((i l1 (+ i 1))) ((> i m) nil)
     (tagbody (fset (fref c i4) (+ (fref c i4) (* sm (dble (fref u 1 i)))))
      (setf i4 (+ i4 ice))
    ))
    label120
  ))
  label130 (go end_label) end_label
  (return
   (values comment
    "     to appear in @solving least squares problems@, prentice-hall, 1974"
))))

