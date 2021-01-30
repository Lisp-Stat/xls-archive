(defun dcgama (mo z w) (declare (type fixnum mo))
 (declare (type (simple-array double-float (*)) z))
 (declare (type (simple-array double-float (*)) w))
 (prog
  ((dpmpar 0.0d0) (drexp 0.0d0) (a 0.0d0) (a1 0.0d0) (a2 0.0d0) (c 0.0d0)
   (cn 0.0d0) (cut 0.0d0) (d 0.0d0) (eps 0.0d0) (et 0.0d0) (e2t 0.0d0)
   (h1 0.0d0) (h2 0.0d0) (q1 0.0d0) (q2 0.0d0) (s 0.0d0) (sn 0.0d0) (s1 0.0d0)
   (s2 0.0d0) (t_ 0.0d0) (t1 0.0d0) (t2 0.0d0) (u 0.0d0) (u1 0.0d0) (u2 0.0d0)
   (v1 0.0d0) (v2 0.0d0) (w1 0.0d0) (w2 0.0d0) (x 0.0d0) (y 0.0d0) (y2 0.0d0)
   (c0 (make-array '(30) :element-type 'double-float)) (dlpi 0.0d0)
   (hl2p 0.0d0) (pi_ 0.0d0) (pi2 0.0d0) (j 0) (nm1 0) (|1.0D| 0.0d0) (n 0)
   (k 0)
  )
  (declare (type double-float dpmpar)) (declare (type double-float drexp))
  (declare (type double-float a)) (declare (type double-float a1))
  (declare (type double-float a2)) (declare (type double-float c))
  (declare (type double-float cn)) (declare (type double-float cut))
  (declare (type double-float d)) (declare (type double-float eps))
  (declare (type double-float et)) (declare (type double-float e2t))
  (declare (type double-float h1)) (declare (type double-float h2))
  (declare (type double-float q1)) (declare (type double-float q2))
  (declare (type double-float s)) (declare (type double-float sn))
  (declare (type double-float s1)) (declare (type double-float s2))
  (declare (type double-float t_)) (declare (type double-float t1))
  (declare (type double-float t2)) (declare (type double-float u))
  (declare (type double-float u1)) (declare (type double-float u2))
  (declare (type double-float v1)) (declare (type double-float v2))
  (declare (type double-float w1)) (declare (type double-float w2))
  (declare (type double-float x)) (declare (type double-float y))
  (declare (type double-float y2))
  (declare (type (simple-array double-float (*)) c0))
  (declare (type double-float dlpi)) (declare (type double-float hl2p))
  (declare (type double-float pi_)) (declare (type double-float pi2))
  (declare (type fixnum j)) (declare (type fixnum nm1))
  (declare (type double-float |1.0D|)) (declare (type fixnum n))
  (declare (type fixnum k)) (setq pi 3.141592653589793d0)
  (setq pi2 6.283185307179586d0) (setq dlpi 1.1447298858494002d0)
  (setq hl2p 0.9189385332046728d0)
  (replace c0 '((+ (- |0.1392432216905901116427432216905901116427D|) 1)) :end 0
  )
  (replace c0 '((+ |0.1796443723688305731649384900158893966944D| 0)) :end 0)
  (replace c0 '((+ (- |0.2955065359477124183006535947712418300654D|) (- 1)))
   :end 0
  )
  (replace c0 '((+ |0.6410256410256410256410256410256410256410D| (- 2))) :end 0
  )
  (replace c0 '((+ (- |0.1917526917526917526917526917526917526918D|) (- 2)))
   :end 0
  )
  (replace c0 '((+ |0.8417508417508417508417508417508417508418D| (- 3))) :end 0
  )
  (replace c0 '((+ (- |0.5952380952380952380952380952380952380952D|) (- 3)))
   :end 0
  )
  (replace c0 '((+ |0.7936507936507936507936507936507936507937D| (- 3))) :end 0
  )
  (replace c0 '((+ (- |0.2777777777777777777777777777777777777778D|) (- 2)))
   :end 0
  )
  (replace c0 '((+ |0.8333333333333333333333333333333333333333D| (- 1))) :end 0
  )
  (replace c0 '((+ (- |0.1236960214226927445425171034927132488108D|) 14)) :end
   0
  )
  (replace c0 '((+ |0.3473202837650022522522522522522522522523D| 12)) :end 0)
  (replace c0 '((+ (- |0.1088226603578439108901514916552510537473D|) 11)) :end
   0
  )
  (replace c0 '((+ |0.3829007513914141414141414141414141414141D| 9)) :end 0)
  (replace c0 '((+ (- |0.1523822153940741619228336495888678051866D|) 8)) :end 0
  )
  (replace c0 '((+ |0.6914722688513130671083952507756734675533D| 6)) :end 0)
  (replace c0 '((+ (- |0.3610877125372498935717326521924223073648D|) 5)) :end 0
  )
  (replace c0 '((+ |0.2193103333333333333333333333333333333333D| 4)) :end 0)
  (replace c0 '((+ (- |0.1568482846260020173063651324520889738281D|) 3)) :end 0
  )
  (replace c0 '((+ |0.1340286404416839199447895100069013112491D| 2)) :end 0)
  (replace c0 '((+ (- |0.6045183405995856967743148238754547286066D|) 31)) :end
   0
  )
  (replace c0 '((+ |0.7218822595185610297836050187301637922490D| 29)) :end 0)
  (replace c0 '((+ (- |0.9252847176120416307230242348347622779519D|) 27)) :end
   0
  )
  (replace c0 '((+ |0.1276337403382883414923495137769782597654D| 26)) :end 0)
  (replace c0 '((+ (- |0.1899991742639920405029371429306942902947D|) 24)) :end
   0
  )
  (replace c0 '((+ |0.3061578263704883415043151051329622758194D| 22)) :end 0)
  (replace c0 '((+ (- |0.5357547217330020361082770919196920448485D|) 20)) :end
   0
  )
  (replace c0 '((+ |0.1021775296525700077565287628053585500394D| 19)) :end 0)
  (replace c0 '((+ (- |0.2132033396091937389697505898213683855747D|) 17)) :end
   0
  )
  (replace c0 '((+ |0.4887880647930793350758151625180229021085D| 15)) :end 0)
  (setf max (ipmpar 3)) (setf eps (dpmpar 1)) (setf x (fref z 1))
  (setf y (fref z 2)) (if (>= x 0.0d0) (go label50)) (setf y (dabs y))
  (setf t_ (* (* -1 pi_) y)) (setf et (dexp t_)) (setf e2t (* et et))
  (setf a1 (* 0.5d0 (+ 1.0d0 e2t))) (setf t2 (+ t_ t_))
  (if (< t2 (- 0.15d0)) (go label10)) (setf a2 (* (* -1 0.5d0) (drexp t2)))
  (go label20) label10 (setf a2 (* 0.5d0 (+ 0.5d0 (+ 0.5d0 (- e2t))))) label20
  (setf u max) (if (>= (dabs x) (dmin1 u (/ 1.0d0 eps))) (go label200))
  (setf k (dabs x)) (setf u (+ x k)) (setf k (mod k 2))
  (if (> u (- 0.5d0)) (go label21)) (setf u (+ 0.5d0 (+ 0.5d0 u)))
  (setf k (+ k 1)) label21 (setf u (* pi_ u)) (setf sn (dsin u))
  (setf cn (dcos u)) (if (/= k 1) (go label30)) (setf sn (- sn))
  (setf cn (- cn)) label30 (setf a1 (* sn a1)) (setf a2 (* cn a2))
  (setf a (+ (* a1 a1) (* a2 a2))) (if (= a 0.0d0) (go label200))
  (if (/= mo 0) (go label40)) (setf h1 (/ a1 a)) (setf h2 (/ (* -1 a2) a))
  (setf c (* pi_ et)) (setf h1 (* c h1)) (setf h2 (* c h2)) (go label41)
  label40 (setf h1 (+ (+ dlpi t_) (* (* -1 0.5d0) (dlog a))))
  (setf h2 (- (datan2 a2 a1))) label41 (if (< (fref z 2) 0.0d0) (go label42))
  (setf x (+ 1.0 (- x))) (setf y (- y)) (go label50) label42 (setf h2 (- h2))
  (setf x (+ 1.0 (- x))) label50 (setf w1 0.0d0) (setf w2 0.0d0) (setf n 0)
  (setf t_ x) (setf y2 (* y y)) (setf a (+ (* t_ t_) y2)) (setf cut 225.0d0)
  (if (> eps (+ |1.0D| (- 30))) (setf cut 144.0d0))
  (if (> eps (+ |1.0D| (- 20))) (setf cut 64.0d0)) (if (>= a cut) (go label80))
  (if (= a 0.0d0) (go label200)) label51 (setf n (+ n 1))
  (setf t_ (+ t_ 1.0d0)) (setf a (+ (* t_ t_) y2)) (if (< a cut) (go label51))
  (setf u1 (/ (+ (* x t_) y2) a)) (setf u2 (/ y a)) (setf s1 u1)
  (setf s2 (* n u2)) (if (< n 2) (go label70)) (setf u (/ t_ a))
  (setf nm1 (+ n (- 1)))
  (fdo ((j 1 (+ j 1))) ((> j nm1) nil)
   (tagbody (setf v1 (+ u1 (* j u))) (setf v2 (* (+ n (- j)) u2))
    (setf c (+ (* s1 v1) (* (* -1 s2) v2))) (setf d (+ (* s1 v2) (* s2 v1)))
    (setf s1 c) (setf s2 d)
  ))
  label70 (setf s (+ (* s1 s1) (* s2 s2))) (if (= mo 0) (go label80))
  (setf w1 (* 0.5d0 (dlog s))) (setf w2 (datan2 s2 s1)) label80
  (setf t1 (+ (* 0.5d0 (dlog a)) (- 1.0d0))) (setf t2 (datan2 y t_))
  (setf u (+ x (- 0.5d0))) (setf v1 (+ (+ (* u t1) (- 0.5d0)) (* (* -1 y) t2)))
  (setf v2 (+ (* u t2) (* y t1))) (setf u1 (/ t_ a)) (setf u2 (/ (* -1 y) a))
  (setf q1 (+ (* u1 u1) (* (* -1 u2) u2))) (setf q2 (* (* 2.0d0 u1) u2))
  (setf a1 0.0d0) (setf a2 0.0d0)
  (fdo ((j 1 (+ j 1))) ((> j 30) nil)
   (tagbody (setf t1 a1) (setf t2 a2) (setf a1 (+ a1 (* (fref c0 j) u1)))
    (setf a2 (+ a2 (* (fref c0 j) u2))) (if (/= a1 t1) (go label90))
    (if (= a2 t2) (go label100)) label90
    (setf t1 (+ (* u1 q1) (* (* -1 u2) q2))) (setf t2 (+ (* u1 q2) (* u2 q1)))
    (setf u1 t1) (setf u2 t2)
  ))
  label100 (setf w1 (+ (+ (+ (+ a1 hl2p) (- w1)) v1) (- n)))
  (setf w2 (+ (+ a2 (- w2)) v2)) (if (< (fref z 1) 0.0d0) (go label120))
  (if (/= mo 0) (go label110)) (setf a (dexp w1)) (setf w1 (* a (dcos w2)))
  (setf w2 (* a (dsin w2))) (if (= n 0) (go label140))
  (setf c (/ (+ (* s1 w1) (* s2 w2)) s))
  (setf d (/ (+ (* s1 w2) (* (* -1 s2) w1)) s)) (setf w1 c) (setf w2 d)
  (go label140) label110 (if (> w2 pi_) (go label111))
  (setf k (+ 0.5d0 (/ (* -1 w2) pi2))) (setf w2 (+ w2 (* pi2 k))) (go label140)
  label111 (setf k (+ (/ w2 pi2) (- 0.5d0))) (setf u (+ k 1))
  (setf w2 (+ w2 (* (* -1 pi2) u))) (if (<= w2 (- pi_)) (setf w2 pi_))
  (go label140) label120 (if (= mo 0) (go label130)) (setf w1 (+ h1 (- w1)))
  (setf w2 (+ h2 (- w2))) (go label110) label130 (setf a (dexp (- w1)))
  (setf t1 (* a (dcos (- w2)))) (setf t2 (* a (dsin (- w2))))
  (setf w1 (+ (* h1 t1) (* (* -1 h2) t2))) (setf w2 (+ (* h1 t2) (* h2 t1)))
  (if (= n 0) (go label140)) (setf c (+ (* w1 s1) (* (* -1 w2) s2)))
  (setf d (+ (* w1 s2) (* w2 s1))) (setf w1 c) (setf w2 d) label140
  (fset (fref w 1) w1) (fset (fref w 2) w2) (go end_label) label200
  (fset (fref w 1) 0.0d0) (fset (fref w 2) 0.0d0) (go end_label) end_label
  (return (values mo z w))
))

