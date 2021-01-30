(proclaim
 '(special npardm kntdim nbreak edist level smooth degree dbreak bright bleft
   xbreak charf norm accur b a knots error buffer discrd right nstack npar
   maxstk maxpar maxknt maxaux left interp ibreak factor both break xright
   xleft errori dsctol rightx nintrp leftx xintrp xdd fright fleft fintrp
   ddtemp
) )
(declare (type fixnum npardm))
(declare (type fixnum kntdim))
(declare (type fixnum nbreak))
(declare (type fixnum edist))
(declare (type fixnum level))
(declare (type fixnum smooth))
(declare (type fixnum degree))
(declare (type (simple-array double-float (20)) dbreak))
(declare (type (simple-array double-float (20)) bright))
(declare (type (simple-array double-float (20)) bleft))
(declare (type (simple-array double-float (20)) xbreak))
(declare (type double-float charf))
(declare (type double-float norm))
(declare (type double-float accur))
(declare (type double-float b))
(declare (type double-float a))
(declare (type fixnum knots))
(declare (type double-float error))
(declare (type double-float buffer))
(declare (type t discrd))
(declare (type fixnum right))
(declare (type fixnum nstack))
(declare (type fixnum npar))
(declare (type fixnum maxstk))
(declare (type fixnum maxpar))
(declare (type fixnum maxknt))
(declare (type fixnum maxaux))
(declare (type fixnum left))
(declare (type fixnum interp))
(declare (type fixnum ibreak))
(declare (type (simple-array double-float (20)) factor))
(declare (type fixnum both))
(declare (type fixnum break))
(declare (type (simple-array double-float (50)) xright))
(declare (type (simple-array double-float (50)) xleft))
(declare (type double-float errori))
(declare (type double-float dsctol))
(declare (type fixnum rightx))
(declare (type fixnum nintrp))
(declare (type fixnum leftx))
(declare (type (simple-array double-float (18)) xintrp))
(declare (type (simple-array double-float (20)) xdd))
(declare (type (simple-array double-float (10)) fright))
(declare (type (simple-array double-float (10)) fleft))
(declare (type (simple-array double-float (18)) fintrp))
(declare (type (simple-array double-float (20 20)) ddtemp))

(defun adput (xknots coefs kdimen kmax ndimen ierr)
 (declare (type (simple-array double-float (*)) xknots))
 (declare (type (simple-array double-float (* *)) coefs))
 (declare (type fixnum kdimen)) (declare (type fixnum kmax))
 (declare (type fixnum ndimen)) (declare (type fixnum ierr))
 (prog
  ((dx 0.0d0) (half 0.0d0) (one 0.0d0)
   (powers (make-array '(20) :element-type 'double-float)) (p 0.0d0)
   (ratio 0.0d0) (three 0.0d0) (k 0)
  )
  (declare (type double-float dx)) (declare (type double-float half))
  (declare (type double-float one))
  (declare (type (simple-array double-float (*)) powers))
  (declare (type double-float p)) (declare (type double-float ratio))
  (declare (type double-float three)) (declare (type fixnum k))
  (setq half 0.5d0) (if discrd (go label30))
  (if (< nstack maxstk) (go label10)) (setf ierr 4)
  (setf dx (+ (fref xright nstack) (- (fref xleft nstack))))
  (if (> dx charf) (setf ierr 3)) (go end_label) label10
  (setf dx (* (+ (fref xright nstack) (- (fref xleft nstack))) half))
  (setf ratio (/ dx (+ (dabs a) (dabs b)))) (if (> ratio buffer) (go label20))
  (setf ierr 4) (go end_label) label20 (setf nstack (+ nstack 1))
  (fset (fref xleft nstack) (fref xleft (+ nstack (- 1))))
  (fset (fref xleft (+ nstack (- 1))) (+ (fref xright (+ nstack (- 1))) (- dx))
  )
  (fset (fref xright nstack) (fref xleft (+ nstack (- 1)))) (go end_label)
  label30 (setf p (dabs norm))
  (if (= norm three) (setf error (dmax1 error errori)))
  (if (/= norm three) (setf error (expt (+ (expt error p) errori) (/ one p))))
  (if (or (= break left) (= break both)) (setf ibreak (+ ibreak 1)))
  (multiple-value-setq (ddtemp powers) (adtran ddtemp powers))
  (fdo ((k 1 (+ k 1))) ((> k npar) nil)
   (tagbody (fset (fref coefs knots k) (fref powers k)))
  )
  (setf knots (+ knots 1)) (fset (fref xknots knots) (fref xright nstack))
  (setf nstack (+ nstack (- 1))) (go end_label) end_label
  (return (values xknots coefs kdimen kmax ndimen ierr))
))
