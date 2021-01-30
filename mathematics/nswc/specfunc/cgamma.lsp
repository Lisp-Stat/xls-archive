(defun cgamma (mo z w) (declare (type fixnum mo)) (declare (type complex z))
 (declare (type complex w))
 (prog
  ((c0 (make-array '(12) :element-type 'float)) (eta 0.0d0) (eta2 0.0d0)
   (sum 0.0d0) (pi2 0.0d0) (l 0) (m 0) (t1 0.0d0) (s 0.0d0) (d 0.0d0)
   (v2 0.0d0) (v1 0.0d0) (j 0) (nm1 0) (s2 0.0d0) (s1 0.0d0) (u2 0.0d0)
   (u1 0.0d0) (cut 0.0d0) (y2 0.0d0) (n 0) (w2 0.0d0) (w1 0.0d0) (alpi 0.0d0)
   (c 0.0d0) (h2 0.0d0) (h1 0.0d0) (a 0.0d0) (cn 0.0d0) (sn 0.0d0) (u 0.0d0)
   (k 0) (a2 0.0d0) (t2 0.0d0) (a1 0.0d0) (e2t 0.0d0) (et 0.0d0) (pi_ 0.0d0)
   (t_ 0.0d0) (y 0.0d0) (x 0.0d0) (eps 0.0d0)
  )
  (declare (type (simple-array float (*)) c0)) (declare (type complex eta))
  (declare (type complex eta2)) (declare (type complex sum))
  (declare (type double-float pi2)) (declare (type fixnum l))
  (declare (type fixnum m)) (declare (type double-float t1))
  (declare (type double-float s)) (declare (type double-float d))
  (declare (type double-float v2)) (declare (type double-float v1))
  (declare (type fixnum j)) (declare (type fixnum nm1))
  (declare (type double-float s2)) (declare (type double-float s1))
  (declare (type double-float u2)) (declare (type double-float u1))
  (declare (type double-float cut)) (declare (type double-float y2))
  (declare (type fixnum n)) (declare (type double-float w2))
  (declare (type double-float w1)) (declare (type double-float alpi))
  (declare (type double-float c)) (declare (type double-float h2))
  (declare (type double-float h1)) (declare (type double-float a))
  (declare (type double-float cn)) (declare (type double-float sn))
  (declare (type double-float u)) (declare (type fixnum k))
  (declare (type double-float a2)) (declare (type double-float t2))
  (declare (type double-float a1)) (declare (type double-float e2t))
  (declare (type double-float et)) (declare (type double-float pi_))
  (declare (type double-float t_)) (declare (type double-float y))
  (declare (type double-float x)) (declare (type double-float eps))
  (setq pi 3.1415927) (setq pi2 6.2831855) (setq alpi 1.1447299)
  (setq hl2p 0.9189385) (replace c0 '((- 156.84828)) :end 0)
  (replace c0 '(13.402864) :end 0) (replace c0 '((- 1.3924322)) :end 0)
  (replace c0 '(0.17964438) :end 0) (replace c0 '((- 0.029550653)) :end 0)
  (replace c0 '(0.0064102565) :end 0) (replace c0 '((- 0.0019175269)) :end 0)
  (replace c0 '(8.417508E-4) :end 0) (replace c0 '((- 5.952381E-4)) :end 0)
  (replace c0 '(7.936508E-4) :end 0) (replace c0 '((- 0.0027777778)) :end 0)
  (replace c0 '(0.083333336) :end 0) (setf max (ipmpar 3))
  (setf eps (spmpar 1)) (setf x (real z)) (setf y (aimag z))
  (if (>= x 0.0) (go label50)) (setf y (abs y)) (setf t_ (* (* -1 pi_) y))
  (setf et (exp t_)) (setf e2t (* et et)) (setf a1 (* 0.5 (+ 1.0 e2t)))
  (setf t2 (+ t_ t_)) (if (< t2 (- 0.15)) (go label10))
  (setf a2 (* (* -1 0.5) (rexp t2))) (go label20) label10
  (setf a2 (* 0.5 (+ 0.5 (+ 0.5 (- e2t))))) label20
  (if (>= (abs x) (amin1 (float max) (/ 1.0 eps))) (go label200))
  (setf k (abs x)) (setf u (+ x k)) (setf k (mod k 2))
  (if (> u (- 0.5)) (go label21)) (setf u (+ 0.5 (+ 0.5 u))) (setf k (+ k 1))
  label21 (setf u (* pi_ u)) (setf sn (sin u)) (setf cn (cos u))
  (if (/= k 1) (go label30)) (setf sn (- sn)) (setf cn (- cn)) label30
  (setf a1 (* sn a1)) (setf a2 (* cn a2)) (setf a (+ (* a1 a1) (* a2 a2)))
  (if (= a 0.0) (go label200)) (if (/= mo 0) (go label40)) (setf h1 (/ a1 a))
  (setf h2 (/ (* -1 a2) a)) (setf c (* pi_ et)) (setf h1 (* c h1))
  (setf h2 (* c h2)) (go label41) label40
  (setf h1 (+ (+ alpi t_) (* (* -1 0.5) (alog a)))) (setf h2 (- (atan2 a2 a1)))
  label41 (if (< (aimag z) 0.0) (go label42)) (setf x (+ 1.0 (- x)))
  (setf y (- y)) (go label50) label42 (setf h2 (- h2)) (setf x (+ 1.0 (- x)))
  label50 (setf w1 0.0) (setf w2 0.0) (setf n 0) (setf t_ x) (setf y2 (* y y))
  (setf a (+ (* t_ t_) y2)) (setf cut 36.0) (if (> eps 1.0E-8) (setf cut 16.0))
  (if (>= a cut) (go label80)) (if (= a 0.0) (go label200)) label51
  (setf n (+ n 1)) (setf t_ (+ t_ 1.0)) (setf a (+ (* t_ t_) y2))
  (if (< a cut) (go label51)) (setf u1 (/ (+ (* x t_) y2) a)) (setf u2 (/ y a))
  (setf s1 u1) (setf s2 (* n u2)) (if (< n 2) (go label70)) (setf u (/ t_ a))
  (setf nm1 (+ n (- 1)))
  (fdo ((j 1 (+ j 1))) ((> j nm1) nil)
   (tagbody (setf v1 (+ u1 (* j u))) (setf v2 (* (+ n (- j)) u2))
    (setf c (+ (* s1 v1) (* (* -1 s2) v2))) (setf d (+ (* s1 v2) (* s2 v1)))
    (setf s1 c) (setf s2 d)
  ))
  label70 (setf s (+ (* s1 s1) (* s2 s2))) (if (= mo 0) (go label80))
  (setf w1 (* 0.5 (alog s))) (setf w2 (atan2 s2 s1)) label80
  (setf t1 (+ (* 0.5 (alog a)) (- 1.0))) (setf t2 (atan2 y t_))
  (setf u (+ x (- 0.5))) (setf v1 (+ (+ (* u t1) (- 0.5)) (* (* -1 y) t2)))
  (setf v2 (+ (* u t2) (* y t1))) (setf eta (cmplx (/ t_ a) (/ (* -1 y) a)))
  (setf eta2 (* eta eta)) (setf m 12) (if (>= a 289.0) (setf m 6))
  (if (> eps 1.0E-8) (setf m (/ m 2))) (setf sum (cmplx (fref c0 m) 0.0))
  (setf l m)
  (fdo ((j 2 (+ j 1))) ((> j m) nil)
   (tagbody (setf l (+ l (- 1)))
    (setf sum (+ (cmplx (fref c0 l) 0.0) (* sum eta2)))
  ))
  (setf sum (* sum eta)) (setf a1 (real sum)) (setf a2 (aimag sum))
  (setf w1 (+ (+ (+ (+ a1 hl2p) (- w1)) v1) (- n)))
  (setf w2 (+ (+ a2 (- w2)) v2)) (if (< (real z) 0.0) (go label120))
  (if (/= mo 0) (go label110)) (setf a (exp w1)) (setf w1 (* a (cos w2)))
  (setf w2 (* a (sin w2))) (if (= n 0) (go label140))
  (setf c (/ (+ (* s1 w1) (* s2 w2)) s))
  (setf d (/ (+ (* s1 w2) (* (* -1 s2) w1)) s)) (setf w1 c) (setf w2 d)
  (go label140) label110 (if (> w2 pi_) (go label111))
  (setf k (+ 0.5 (/ (* -1 w2) pi2))) (setf w2 (+ w2 (* pi2 k))) (go label140)
  label111 (setf k (+ (/ w2 pi2) (- 0.5)))
  (setf w2 (+ w2 (* (* -1 pi2) (float (+ k 1)))))
  (if (<= w2 (- pi_)) (setf w2 pi_)) (go label140) label120
  (if (= mo 0) (go label130)) (setf w1 (+ h1 (- w1))) (setf w2 (+ h2 (- w2)))
  (go label110) label130 (setf a (exp (- w1))) (setf t1 (* a (cos (- w2))))
  (setf t2 (* a (sin (- w2)))) (setf w1 (+ (* h1 t1) (* (* -1 h2) t2)))
  (setf w2 (+ (* h1 t2) (* h2 t1))) (if (= n 0) (go label140))
  (setf c (+ (* w1 s1) (* (* -1 w2) s2))) (setf d (+ (* w1 s2) (* w2 s1)))
  (setf w1 c) (setf w2 d) label140 (setf w (cmplx w1 w2)) (go end_label)
  label200 (setf w (complex 0.0 0.0)) (go end_label) end_label
  (return (values mo z w))
))
