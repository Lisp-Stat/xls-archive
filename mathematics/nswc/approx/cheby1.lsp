(defun cheby1
 (a b f g phi eps itno mxiter l m p q error ierr lp1 mp1 lpm n np1 x xval c d
  err h
 )
 (declare (type double-float a)) (declare (type double-float b))
 (declare (type double-float eps)) (declare (type fixnum itno))
 (declare (type fixnum mxiter)) (declare (type fixnum l))
 (declare (type fixnum m)) (declare (type (simple-array double-float (*)) p))
 (declare (type (simple-array double-float (*)) q))
 (declare (type double-float error)) (declare (type fixnum ierr))
 (declare (type fixnum lp1)) (declare (type fixnum mp1))
 (declare (type fixnum lpm)) (declare (type fixnum n))
 (declare (type fixnum np1)) (declare (type (simple-array double-float (*)) x))
 (declare (type (simple-array double-float (*)) xval))
 (declare (type (simple-array double-float (* *)) c))
 (declare (type (simple-array double-float (*)) d))
 (declare (type (simple-array double-float (*)) err))
 (declare (type (simple-array double-float (*)) h))
 (prog
  ((b1 0.0d0) (c0 0.0d0) (del 0.0d0) (dn 0.0d0) (dnp1 0.0d0) (eps0 0.0d0)
   (half 0.0d0) (h1 0.0d0) (olderr 0.0d0) (one 0.0d0) (pi_ 0.0d0) (sign 0.0d0)
   (sum 0.0d0) (tau 0.0d0) (templ 0.0d0) (ten 0.0d0) (test 0.0d0) (u 0.0d0)
   (xi 0.0d0) (xlb 0.0d0) (xm1 0.0d0) (y 0.0d0) (y2 0.0d0) (y3 0.0d0) (z 0.0d0)
   (zero 0.0d0) (zz 0.0d0) (z1 0.0d0) (z2 0.0d0) (z3 0.0d0) (f 0.0d0) (g 0.0d0)
   (phi 0.0d0) (lpi 0) (j 0) (kount 0) (ii 0) (k 0) (i 0)
  )
  (declare (type double-float b1)) (declare (type double-float c0))
  (declare (type double-float del)) (declare (type double-float dn))
  (declare (type double-float dnp1)) (declare (type double-float eps0))
  (declare (type double-float half)) (declare (type double-float h1))
  (declare (type double-float olderr)) (declare (type double-float one))
  (declare (type double-float pi_)) (declare (type double-float sign))
  (declare (type double-float sum)) (declare (type double-float tau))
  (declare (type double-float templ)) (declare (type double-float ten))
  (declare (type double-float test)) (declare (type double-float u))
  (declare (type double-float xi)) (declare (type double-float xlb))
  (declare (type double-float xm1)) (declare (type double-float y))
  (declare (type double-float y2)) (declare (type double-float y3))
  (declare (type double-float z)) (declare (type double-float zero))
  (declare (type double-float zz)) (declare (type double-float z1))
  (declare (type double-float z2)) (declare (type double-float z3))
  (declare (type double-float f)) (declare (type double-float g))
  (declare (type double-float phi)) (declare (type fixnum lpi))
  (declare (type fixnum j)) (declare (type fixnum kount))
  (declare (type fixnum ii)) (declare (type fixnum k))
  (declare (type fixnum i)) (setq pi 3.141592653589793d0) (setq ten 10.0d0)
  (setq one 1.0d0) (setq half 0.5d0) (setq zero 0.0d0) (setq c0 0.0625d0)
  (setq tau 0.015d0) (setq eps0 (+ |1.0D| (- 2))) (setf error zero)
  (if (or (<= eps zero) (>= eps eps0)) (go label200)) (setf ierr 0)
  (setf itno 1) (setf xlb zero) (setf dn n) (setf dnp1 np1)
  (fdo ((i 1 (+ i 1))) ((> i lp1) nil) (tagbody (fset (fref p i) zero)))
  (fdo ((i 1 (+ i 1))) ((> i mp1) nil) (tagbody (fset (fref q i) zero)))
  (fset (fref q 1) one) (fset (fref x 1) a) (fset (fref x np1) b)
  (setf k (/ n 2)) (if (<= k 0) (go label30)) (setf b1 (* half (+ b (- a))))
  (setf xm1 (* half (+ a b)))
  (fdo ((i 1 (+ i 1))) ((> i k) nil)
   (tagbody (setf xi i) (setf z (* (* -1 b1) (dcos (* pi_ (/ xi dn)))))
    (fset (fref x (+ i 1)) (+ z xm1)) (setf ii (+ np1 (- i)))
    (fset (fref x ii) (+ xm1 (- z)))
  ))
  label30
  (fdo ((i 1 (+ i 1))) ((> i np1) nil)
   (tagbody (fset (fref xval i) (funcall (phi (fref x i)))))
  )
  (setf kount 1) label40 (setf k (+ l 2)) (setf sign one)
  (fdo ((i 1 (+ i 1))) ((> i np1) nil)
   (tagbody (setf sign (- sign)) (fset (fref c i 1) one)
    (if (<= l 0) (go label42))
    (fdo ((j 2 (+ j 1))) ((> j lp1) nil)
     (tagbody (fset (fref c i j) (* (fref c i (+ j (- 1))) (fref xval i))))
    )
    label42 (fset (fref d i) (funcall (f (fref x i))))
    (if (<= m 0) (go label44))
    (setf templ (+ (* (* sign xlb) (funcall (g (fref x i)))) (- (fref d i))))
    (fset (fref c i k) (* (fref xval i) templ)) (if (> k lpm) (go label44))
    (fdo ((j k (+ j 1))) ((> j lpm) nil)
     (tagbody (fset (fref c i (+ j 1)) (* (fref c i j) (fref xval i))))
    )
    label44 (fset (fref c i np1) (* sign (funcall (g (fref x i)))))
  ))
  (multiple-value-setq (np1 dummy_var c np1 d np1 ierr)
   (dpslv np1 1 c np1 d np1 ierr)
  )
  (if (/= ierr 0) (go label220)) (if (> kount 1) (go label50))
  (setf xlb (/ (+ (fref d np1) (* xlb dn)) dnp1)) (if (<= m 0) (go label61))
  (setf kount 2) (go label40) label50
  (setf test (dabs (+ xlb (- (fref d np1)))))
  (setf xlb (/ (+ (fref d np1) (* xlb dn)) dnp1)) (setf kount (+ kount 1))
  (if (and (<= kount 4) (> test (* eps0 (dabs xlb)))) (go label40))
  (fdo ((i 2 (+ i 1))) ((> i mp1) nil)
   (tagbody (setf lpi (+ l i)) (fset (fref q i) (fref d lpi)))
  )
  label61
  (fdo ((i 1 (+ i 1))) ((> i lp1) nil) (tagbody (fset (fref p i) (fref d i))))
  (setf olderr error) (setf error zero) (setf z1 zero) (setf u one)
  (if (< xlb zero) (setf u (- u))) (if (> n 1) (go label70))
  (fset (fref h 1) (* tau (+ (fref x 2) (- (fref x 1)))))
  (fset (fref h 2) (- (fref h 1))) (go label72) label70
  (fdo ((i 2 (+ i 1))) ((> i n) nil)
   (tagbody
    (fset (fref h i) (* tau (+ (fref x (+ i 1)) (- (fref x (+ i (- 1)))))))
  ))
  (fset (fref h 1) (* half (fref h 2)))
  (fset (fref h np1) (* (* -1 half) (fref h n))) label72
  (fdo ((i 1 (+ i 1))) ((> i np1) nil)
   (tagbody (setf y2 (fref x i)) (setf h1 (fref h i)) (setf y3 (+ y2 h1))
    (multiple-value-setq
     (y2 dummy_var dummy_var dummy_var del ierr l lp1 m np1 d)
     (cerr y2 (funcall (f y2)) (funcall (g y2)) (funcall (phi y2)) del ierr l
      lp1 m np1 d
    ))
    (if (/= ierr 0) (go end_label)) (setf z2 (* u del))
    (multiple-value-setq
     (y3 dummy_var dummy_var dummy_var del ierr l lp1 m np1 d)
     (cerr y3 (funcall (f y3)) (funcall (g y3)) (funcall (phi y3)) del ierr l
      lp1 m np1 d
    ))
    (if (/= ierr 0) (go end_label)) (setf z3 (* u del))
    (if (< z2 z3) (go label80)) (setf h1 (- h1)) (setf z z3) (setf z3 z2)
    (setf z2 z) (setf y y3) (setf y3 y2) (setf y2 y) label80 (setf y (+ y3 h1))
    (if (>= y a) (go label81)) (setf y a) (go label90) label81
    (if (<= y b) (go label82)) (setf y b) (go label90) label82
    (multiple-value-setq
     (y dummy_var dummy_var dummy_var del ierr l lp1 m np1 d)
     (cerr y (funcall (f y)) (funcall (g y)) (funcall (phi y)) del ierr l lp1 m
      np1 d
    ))
    (if (/= ierr 0) (go end_label)) (setf z (* u del))
    (if (<= z z3) (go label83)) (setf y2 y3) (setf y3 y) (setf z2 z3)
    (setf z3 z) (go label80) label83 (setf y (+ (+ z (- z3)) (+ z2 (- z3))))
    (if (/= y zero) (go label84)) (setf y y3) (go label90) label84
    (setf y (+ (* half (+ y2 y3)) (/ (* h1 (+ z2 (- z3))) y))) label90
    (fset (fref x i) y)
    (multiple-value-setq
     (y dummy_var dummy_var dummy_var del ierr l lp1 m np1 d)
     (cerr y (funcall (f y)) (funcall (g y)) (funcall (phi y)) del ierr l lp1 m
      np1 d
    ))
    (if (/= ierr 0) (go end_label)) (fset (fref err i) del) (setf u (- u))
    (if (= i 1) (go label91))
    (if (<= (fref x i) (fref x (+ i (- 1)))) (go label230)) label91
    (setf z (dabs (fref err i))) (setf error (dmax1 error z))
    (if (>= z ten) (go label240)) (setf y (dabs xlb)) (setf zz one)
    (if (/= y zero) (setf zz (/ (dabs (+ z (- y))) y)))
    (if (< z1 zz) (setf z1 zz))
  ))
  (if (<= (fref x 1) a) (go label110)) (setf h1 (* c0 (+ (fref x 1) (- a))))
  (setf u one) (if (>= xlb zero) (setf u (- u))) (setf z3 zero) (setf y a)
  (fdo ((i 1 (+ i 1))) ((> i 16) nil)
   (tagbody
    (multiple-value-setq
     (y dummy_var dummy_var dummy_var del ierr l lp1 m np1 d)
     (cerr y (funcall (f y)) (funcall (g y)) (funcall (phi y)) del ierr l lp1 m
      np1 d
    ))
    (if (/= ierr 0) (go end_label)) (setf z (* u del))
    (if (<= z z3) (go label100)) (setf z3 z) (setf z2 y) label100
    (setf y (+ y h1))
  ))
  (setf error (dmax1 error z3)) (setf z (dabs xlb))
  (if (<= z3 z) (go label110)) (setf i np1)
  (fdo ((ii 2 (+ ii 1))) ((> ii np1) nil)
   (tagbody (fset (fref err i) (fref err (+ i (- 1))))
    (fset (fref x i) (fref x (+ i (- 1)))) (setf i (+ i (- 1)))
  ))
  (fset (fref x 1) z2) (fset (fref err 1) (* u z3)) (go label113) label110
  (if (>= (fref x np1) b) (go label120))
  (setf h1 (* c0 (+ b (- (fref x np1))))) (setf u one)
  (if (>= (fref err np1) zero) (setf u (- u))) (setf z3 zero) (setf y b)
  (fdo ((i 1 (+ i 1))) ((> i 16) nil)
   (tagbody
    (multiple-value-setq
     (y dummy_var dummy_var dummy_var del ierr l lp1 m np1 d)
     (cerr y (funcall (f y)) (funcall (g y)) (funcall (phi y)) del ierr l lp1 m
      np1 d
    ))
    (if (/= ierr 0) (go end_label)) (setf z (* u del))
    (if (<= z z3) (go label111)) (setf z3 z) (setf z2 y) label111
    (setf y (+ y (- h1)))
  ))
  (setf error (dmax1 error z3)) (setf z (dabs xlb))
  (if (<= z3 z) (go label120))
  (fdo ((i 1 (+ i 1))) ((> i n) nil)
   (tagbody (fset (fref err i) (fref err (+ i 1)))
    (fset (fref x i) (fref x (+ i 1)))
  ))
  (fset (fref x np1) z2) (fset (fref err np1) (* u z3)) label113
  (setf xlb (- xlb)) (setf zz one)
  (if (/= z zero) (setf zz (/ (dabs (+ z3 (- z))) z)))
  (if (< z1 zz) (setf z1 zz)) label120 (if (<= z1 eps) (go end_label))
  (if (>= itno mxiter) (go label210)) (setf sum zero) (setf sign one)
  (fdo ((i 1 (+ i 1))) ((> i np1) nil)
   (tagbody (setf sum (+ sum (* sign (fref err i)))) (setf sign (- sign)))
  )
  (setf xlb (/ sum dnp1)) (setf itno (+ itno 1)) (go label30) label200
  (setf ierr 1) (go end_label) label210 (setf ierr 2) (go end_label) label220
  (if (= itno 1) (go label250)) (setf ierr 3) (go end_label) label230
  (setf ierr 4) (if (<= i n) (setf error olderr)) (go end_label) label240
  (setf ierr 5) (go end_label) label250 (setf ierr 6) (go end_label) end_label
  (return
   (values a b f g phi eps itno mxiter l m p q error ierr lp1 mp1 lpm n np1 x
    xval c d err h
))))

