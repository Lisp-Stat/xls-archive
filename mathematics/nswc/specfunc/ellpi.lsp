(defun ellpi (phi cphi k l f e ierr) (declare (type double-float phi))
 (declare (type double-float cphi)) (declare (type float k))
 (declare (type float l)) (declare (type double-float f))
 (declare (type double-float e)) (declare (type fixnum ierr))
 (prog
  ((k2 0.0d0) (l2 0.0d0) (ln4 0.0d0) (t2 0.0d0) (t1 0.0d0) (p 0.0d0) (w 0.0d0)
   (d4 0.0d0) (d3 0.0d0) (d2 0.0d0) (d1 0.0d0) (r0 0.0d0) (rk 0.0d0) (rj 0.0d0)
   (ri 0.0d0) (dn 0.0d0) (td 0.0d0) (s4 0.0d0) (s3 0.0d0) (rn 0.0d0) (rm 0.0d0)
   (sk 0.0d0) (sj 0.0d0) (si 0.0d0) (r2 0.0d0) (s0 0.0d0) (r 0.0d0) (ts 0.0d0)
   (tr 0.0d0) (s2 0.0d0) (s1 0.0d0) (hn 0.0d0) (an 0.0d0) (qn 0.0d0) (pn 0.0d0)
   (qx 0.0d0) (px 0.0d0) (ss 0.0d0) (cn 0.0d0) (sn 0.0d0)
  )
  (declare (type float k2)) (declare (type float l2))
  (declare (type float ln4)) (declare (type double-float t2))
  (declare (type double-float t1)) (declare (type double-float p))
  (declare (type double-float w)) (declare (type double-float d4))
  (declare (type double-float d3)) (declare (type double-float d2))
  (declare (type double-float d1)) (declare (type double-float r0))
  (declare (type double-float rk)) (declare (type double-float rj))
  (declare (type double-float ri)) (declare (type double-float dn))
  (declare (type double-float td)) (declare (type double-float s4))
  (declare (type double-float s3)) (declare (type double-float rn))
  (declare (type double-float rm)) (declare (type double-float sk))
  (declare (type double-float sj)) (declare (type double-float si))
  (declare (type double-float r2)) (declare (type double-float s0))
  (declare (type double-float r)) (declare (type double-float ts))
  (declare (type double-float tr)) (declare (type double-float s2))
  (declare (type double-float s1)) (declare (type double-float hn))
  (declare (type double-float an)) (declare (type double-float qn))
  (declare (type double-float pn)) (declare (type double-float qx))
  (declare (type double-float px)) (declare (type double-float ss))
  (declare (type double-float cn)) (declare (type double-float sn))
  (setq ln4 1.3862944) (setq th1 0.7615942)
  (if (or (< phi 0.0) (< cphi 0.0)) (go label100))
  (if (or (> (abs k) 1.0) (> (abs l) 1.0)) (go label110)) (setf ierr 0)
  (if (/= phi 0.0) (go label10)) (setf f 0.0) (setf e 0.0) (go end_label)
  label10 (if (< phi 0.79) (go label11)) (setf sn (cos cphi))
  (setf cn (sin cphi)) (go label20) label11 (setf sn (sin phi))
  (setf cn (cos phi)) label20 (setf k2 (* k k)) (setf l2 (* l l))
  (setf ss (* sn sn)) (setf px (abs (* k sn))) (setf qx (abs (* k cn)))
  (if (>= px th1) (go label50)) (setf pn 1.0) (setf qn 2.0) (setf an phi)
  (setf hn 1.0) (setf s1 0.0) (setf s2 0.0) (setf tr (* phi ss))
  (setf ts (* sn cn)) label30 (setf an (/ (+ (* pn an) (- ts)) qn))
  (setf r (/ (* k2 hn) qn)) (setf s2 (+ s2 (* r an))) (setf hn (* pn r))
  (setf s0 s1) (setf s1 (+ s1 (* hn an)))
  (if (< (abs tr) (abs an)) (go label40))
  (if (<= (abs s1) (abs s0)) (go label40)) (setf pn (+ qn 1.0))
  (setf qn (+ pn 1.0)) (setf tr (* ss tr)) (setf ts (* ss ts)) (go label30)
  label40 (setf f (+ phi s1)) (setf e (+ phi (- s2))) (go end_label) label50
  (setf r (cpabs l qx)) (if (= r 0.0) (go label120)) (setf r2 (* r r))
  (setf si 1.0) (setf sj 1.0) (setf sk 0.0) (setf rm 0.0) (setf rn 0.0)
  (setf s1 0.0) (setf s2 0.0) (setf s3 0.0) (setf s4 0.0) (setf td (* qx r))
  (setf dn 2.0) (go label70) label60 (setf si ri) (setf sj rj) (setf sk rk)
  (setf dn (+ dn 2.0)) (setf td (* r2 td)) label70
  (setf pn (/ (+ dn (- 1.0)) dn)) (setf qn (/ (+ dn 1.0) (+ dn 2.0)))
  (setf ri (* pn si)) (setf rj (* (* (* pn pn) l2) sj))
  (setf rk (+ sk (/ 2.0 (* dn (+ dn (- 1.0)))))) (setf r0 (/ td dn))
  (setf rm (* (* (* qn qn) l2) (+ rm (* (* -1 r0) ri))))
  (setf rn (* (* (* pn qn) l2) (+ rn (* (* -1 r0) si)))) (setf d1 rj)
  (setf d2 (* qn rj)) (setf d3 (+ rm (* (* -1 rj) rk)))
  (setf d4 (+ (+ rn (* (* (* (* -1 pn) l2) sj) rk)) (/ (* l2 sj) (* dn dn))))
  (setf r0 s3) (setf s1 (+ s1 d1)) (setf s2 (+ s2 d2)) (setf s3 (+ s3 d3))
  (setf s4 (+ s4 d4)) (if (< s3 r0) (go label60)) (setf w (+ 1.0 px))
  (setf p (+ ln4 (- (alog (+ r qx)))))
  (setf t1 (+ (* (+ 1.0 s1) p) (* (/ qx r) (alnrel (/ (* (* -1 0.5) r2) w)))))
  (setf t2 (+ (* (* (+ 0.5 s2) l2) p) (+ 1.0 (/ (* (* -1 qx) r) w))))
  (setf f (+ t1 s3)) (setf e (+ t2 s4)) (go end_label) label100 (setf ierr 1)
  (go end_label) label110 (setf ierr 2) (go end_label) label120 (setf ierr 3)
  (go end_label) end_label (return (values phi cphi k l f e ierr))
))

