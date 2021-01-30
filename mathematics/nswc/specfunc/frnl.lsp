(defun frnl (t_ c s) (declare (type double-float t_))
 (declare (type double-float c)) (declare (type double-float s))
 (prog
  ((fp (make-array '(7) :element-type 'float))
   (gp (make-array '(7) :element-type 'float))
   (p (make-array '(6) :element-type 'float))
   (q (make-array '(6) :element-type 'float))
   (cn (make-array '(5) :element-type 'float))
   (cd (make-array '(5) :element-type 'float))
   (dn (make-array '(5) :element-type 'float))
   (dd (make-array '(5) :element-type 'float))
   (an (make-array '(6) :element-type 'float))
   (ad (make-array '(6) :element-type 'float))
   (bn (make-array '(6) :element-type 'float))
   (bd (make-array '(6) :element-type 'float))
   (pn (make-array '(6) :element-type 'float))
   (pd (make-array '(6) :element-type 'float))
   (qn (make-array '(6) :element-type 'float))
   (qd (make-array '(6) :element-type 'float))
   (a (make-array '(6) :element-type 'float))
   (b (make-array '(6) :element-type 'float))
   (cp (make-array '(13) :element-type 'float))
   (sp (make-array '(13) :element-type 'float)) (n 0.0d0) (cy 0.0d0) (sy 0.0d0)
   (r 0.0d0) (l 0) (m 0) (pixx 0.0d0) (pi_ 0.0d0) (pix 0.0d0) (g 0.0d0)
   (f 0.0d0) (gd 0.0d0) (gn 0.0d0) (fd 0.0d0) (fn 0.0d0) (i 0) (y 0.0d0)
   (xx 0.0d0) (x 0.0d0)
  )
  (declare (type (simple-array float (*)) fp))
  (declare (type (simple-array float (*)) gp))
  (declare (type (simple-array float (*)) p))
  (declare (type (simple-array float (*)) q))
  (declare (type (simple-array float (*)) cn))
  (declare (type (simple-array float (*)) cd))
  (declare (type (simple-array float (*)) dn))
  (declare (type (simple-array float (*)) dd))
  (declare (type (simple-array float (*)) an))
  (declare (type (simple-array float (*)) ad))
  (declare (type (simple-array float (*)) bn))
  (declare (type (simple-array float (*)) bd))
  (declare (type (simple-array float (*)) pn))
  (declare (type (simple-array float (*)) pd))
  (declare (type (simple-array float (*)) qn))
  (declare (type (simple-array float (*)) qd))
  (declare (type (simple-array float (*)) a))
  (declare (type (simple-array float (*)) b))
  (declare (type (simple-array float (*)) cp))
  (declare (type (simple-array float (*)) sp)) (declare (type float n))
  (declare (type double-float cy)) (declare (type double-float sy))
  (declare (type double-float r)) (declare (type fixnum l))
  (declare (type fixnum m)) (declare (type double-float pixx))
  (declare (type double-float pi_)) (declare (type double-float pix))
  (declare (type double-float g)) (declare (type double-float f))
  (declare (type double-float gd)) (declare (type double-float gn))
  (declare (type double-float fd)) (declare (type double-float fn))
  (declare (type fixnum i)) (declare (type double-float y))
  (declare (type double-float xx)) (declare (type double-float x))
  (setq pi 3.1415927) (replace a '(1.0) :end 0)
  (replace a '((- 0.2467401)) :end 0) (replace a '(0.028185502) :end 0)
  (replace a '((- 0.0016048831)) :end 0) (replace a '(5.4073065E-5) :end 0)
  (replace a '((- 1.1927824E-6)) :end 0) (replace b '(0.5235988) :end 0)
  (replace b '((- 0.09228059)) :end 0) (replace b '(0.0072447844) :end 0)
  (replace b '((- 3.1211693E-4)) :end 0) (replace b '(8.4441535E-6) :end 0)
  (replace b '((- 1.5565307E-7)) :end 0) (replace cp '(1.0) :end 0)
  (replace cp '((- 0.2467401)) :end 0) (replace cp '(0.028185502) :end 0)
  (replace cp '((- 0.0016048831)) :end 0) (replace cp '(5.4074135E-5) :end 0)
  (replace cp '((- 1.2000972E-6)) :end 0) (replace cp '(1.8843492E-8) :end 0)
  (replace cp '((- 2.2022655E-10)) :end 0) (replace cp '(1.9895496E-12) :end 0)
  (replace cp '((- 1.429795E-14)) :end 0) (replace cp '(8.321257E-17) :end 0)
  (replace cp '((- 3.8444483E-19)) :end 0) (replace cp '(1.1473995E-21) :end 0)
  (replace sp '(0.5235988) :end 0) (replace sp '((- 0.09228059)) :end 0)
  (replace sp '(0.0072447844) :end 0) (replace sp '((- 3.1211693E-4)) :end 0)
  (replace sp '(8.444273E-6) :end 0) (replace sp '((- 1.5647144E-7)) :end 0)
  (replace sp '(2.1082118E-9) :end 0) (replace sp '((- 2.157423E-11)) :end 0)
  (replace sp '(1.7333219E-13) :end 0) (replace sp '((- 1.1216164E-15)) :end 0)
  (replace sp '(5.9411747E-18) :end 0) (replace sp '((- 2.52758E-20)) :end 0)
  (replace sp '(7.0570077E-23) :end 0) (replace pn '(96.79854) :end 0)
  (replace pn '(632.3698) :end 0) (replace pn '(598.79645) :end 0)
  (replace pn '(158.2581) :end 0) (replace pn '(13.49194) :end 0)
  (replace pn '(0.3183098) :end 0) (replace pd '(486.67856) :end 0)
  (replace pd '(2319.1013) :end 0) (replace pd '(2000.3467) :end 0)
  (replace pd '(509.08548) :end 0) (replace pd '(42.690098) :end 0)
  (replace pd '(1.0) :end 0) (replace qn '(36.45666) :end 0)
  (replace qn '(305.0407) :end 0) (replace qn '(274.18384) :end 0)
  (replace qn '(65.20952) :end 0) (replace qn '(4.905347) :end 0)
  (replace qn '(0.10132088) :end 0) (replace qd '(1688.0183) :end 0)
  (replace qd '(5222.1387) :end 0) (replace qd '(3434.7075) :end 0)
  (replace qd '(709.8541) :end 0) (replace qd '(49.933002) :end 0)
  (replace qd '(1.0) :end 0) (replace an '(2679.5574) :end 0)
  (replace an '(8918.319) :end 0) (replace an '(4266.734) :end 0)
  (replace an '(575.0038) :end 0) (replace an '(25.417917) :end 0)
  (replace an '(0.31830987) :end 0) (replace ad '(12042.127) :end 0)
  (replace ad '(30922.842) :end 0) (replace ad '(13884.889) :end 0)
  (replace ad '(1829.7146) :end 0) (replace ad '(80.15671) :end 0)
  (replace ad '(1.0) :end 0) (replace bn '(1306.8066) :end 0)
  (replace bn '(4849.0195) :end 0) (replace bn '(2060.7961) :end 0)
  (replace bn '(240.93202) :end 0) (replace bn '(9.25022) :end 0)
  (replace bn '(0.10132118) :end 0) (replace bd '(41859.31) :end 0)
  (replace bd '(68563.89) :end 0) (replace bd '(23392.445) :end 0)
  (replace bd '(2509.2683) :end 0) (replace bd '(92.81582) :end 0)
  (replace bd '(1.0) :end 0) (replace cn '(2903.1426) :end 0)
  (replace cn '(3945.398) :end 0) (replace cn '(691.42883) :end 0)
  (replace cn '(29.919197) :end 0) (replace cn '(0.31830987) :end 0)
  (replace cd '(11499.143) :end 0) (replace cd '(12972.648) :end 0)
  (replace cd '(2199.773) :end 0) (replace cd '(94.29789) :end 0)
  (replace cd '(1.0) :end 0) (replace dn '(1712.7068) :end 0)
  (replace dn '(2131.3025) :end 0) (replace dn '(306.28232) :end 0)
  (replace dn '(11.0988035) :end 0) (replace dn '(0.10132118) :end 0)
  (replace dd '(35924.19) :end 0) (replace dd '(24934.209) :end 0)
  (replace dd '(3181.9758) :end 0) (replace dd '(111.060616) :end 0)
  (replace dd '(1.0) :end 0) (replace fp '(0.31830987) :end 0)
  (replace fp '((- 0.0967546)) :end 0) (replace fp '(0.3431129) :end 0)
  (replace fp '((- 3.439666)) :end 0) (replace fp '(66.92611) :end 0)
  (replace fp '((- 1887.6365)) :end 0) (replace fp '(44976.34) :end 0)
  (replace gp '(0.10132118) :end 0) (replace gp '((- 0.15398972)) :end 0)
  (replace gp '(0.9829341) :end 0) (replace gp '((- 14.225261)) :end 0)
  (replace gp '(359.16476) :end 0) (replace gp '((- 12061.899)) :end 0)
  (replace gp '(316642.2) :end 0) (replace p '(1.0) :end 0)
  (replace p '((- 3.0)) :end 0) (replace p '(105.0) :end 0)
  (replace p '((- 10395.0)) :end 0) (replace p '(2027025.0) :end 0)
  (replace p '((- 6.547291E8)) :end 0) (replace q '(1.0) :end 0)
  (replace q '((- 15.0)) :end 0) (replace q '(945.0) :end 0)
  (replace q '((- 135135.0)) :end 0) (replace q '(3.4459424E7) :end 0)
  (replace q '((- 1.374931E10)) :end 0) (setf max (ipmpar 3)) (setf x (abs t_))
  (if (> x 4.0) (go label50)) (setf xx (* x x)) (setf y (* xx xx))
  (if (> x 0.6) (go label10))
  (setf c
   (+
    (*
     (+
      (*
       (+ (* (+ (* (+ (* (fref a 1) y) (fref a 2)) y) (fref a 3)) y) (fref a 4)
       )
       y
      )
      (fref a 5)
     )
     y
    )
    (fref a 6)
  ))
  (setf s
   (+
    (*
     (+
      (*
       (+ (* (+ (* (+ (* (fref b 1) y) (fref b 2)) y) (fref b 3)) y) (fref b 4)
       )
       y
      )
      (fref b 5)
     )
     y
    )
    (fref b 6)
  ))
  (setf c (* t_ c)) (setf s (* (* t_ xx) s)) (go end_label) label10
  (if (>= x 1.65) (go label20)) (setf c (fref cp 1)) (setf s (fref sp 1))
  (fdo ((i 2 (+ i 1))) ((> i 13) nil)
   (tagbody (setf c (+ (fref cp i) (* c y))) (setf s (+ (fref sp i) (* s y))))
  )
  (setf c (* t_ c)) (setf s (* (* t_ xx) s)) (go end_label) label20
  (if (>= x 2.0) (go label30))
  (setf fn
   (+
    (*
     (+
      (*
       (+ (* (+ (* (+ (* (fref pn 1) y) (fref pn 2)) y) (fref pn 3)) y)
        (fref pn 4)
       )
       y
      )
      (fref pn 5)
     )
     y
    )
    (fref pn 6)
  ))
  (setf fd
   (+
    (*
     (+
      (*
       (+ (* (+ (* (+ (* (fref pd 1) y) (fref pd 2)) y) (fref pd 3)) y)
        (fref pd 4)
       )
       y
      )
      (fref pd 5)
     )
     y
    )
    (fref pd 6)
  ))
  (setf gn
   (+
    (*
     (+
      (*
       (+ (* (+ (* (+ (* (fref qn 1) y) (fref qn 2)) y) (fref qn 3)) y)
        (fref qn 4)
       )
       y
      )
      (fref qn 5)
     )
     y
    )
    (fref qn 6)
  ))
  (setf gd
   (+
    (*
     (+
      (*
       (+ (* (+ (* (+ (* (fref qd 1) y) (fref qd 2)) y) (fref qd 3)) y)
        (fref qd 4)
       )
       y
      )
      (fref qd 5)
     )
     y
    )
    (fref qd 6)
  ))
  (setf f (/ fn (* x fd))) (setf g (/ gn (* (* x xx) gd))) (setf y (* 0.5 xx))
  (go label80) label30 (if (>= x 3.0) (go label40))
  (setf fn
   (+
    (*
     (+
      (*
       (+ (* (+ (* (+ (* (fref an 1) y) (fref an 2)) y) (fref an 3)) y)
        (fref an 4)
       )
       y
      )
      (fref an 5)
     )
     y
    )
    (fref an 6)
  ))
  (setf fd
   (+
    (*
     (+
      (*
       (+ (* (+ (* (+ (* (fref ad 1) y) (fref ad 2)) y) (fref ad 3)) y)
        (fref ad 4)
       )
       y
      )
      (fref ad 5)
     )
     y
    )
    (fref ad 6)
  ))
  (setf gn
   (+
    (*
     (+
      (*
       (+ (* (+ (* (+ (* (fref bn 1) y) (fref bn 2)) y) (fref bn 3)) y)
        (fref bn 4)
       )
       y
      )
      (fref bn 5)
     )
     y
    )
    (fref bn 6)
  ))
  (setf gd
   (+
    (*
     (+
      (*
       (+ (* (+ (* (+ (* (fref bd 1) y) (fref bd 2)) y) (fref bd 3)) y)
        (fref bd 4)
       )
       y
      )
      (fref bd 5)
     )
     y
    )
    (fref bd 6)
  ))
  (setf f (/ fn (* x fd))) (setf g (/ gn (* (* x xx) gd))) (go label70) label40
  (setf fn
   (+
    (*
     (+ (* (+ (* (+ (* (fref cn 1) y) (fref cn 2)) y) (fref cn 3)) y)
      (fref cn 4)
     )
     y
    )
    (fref cn 5)
  ))
  (setf fd
   (+
    (*
     (+ (* (+ (* (+ (* (fref cd 1) y) (fref cd 2)) y) (fref cd 3)) y)
      (fref cd 4)
     )
     y
    )
    (fref cd 5)
  ))
  (setf gn
   (+
    (*
     (+ (* (+ (* (+ (* (fref dn 1) y) (fref dn 2)) y) (fref dn 3)) y)
      (fref dn 4)
     )
     y
    )
    (fref dn 5)
  ))
  (setf gd
   (+
    (*
     (+ (* (+ (* (+ (* (fref dd 1) y) (fref dd 2)) y) (fref dd 3)) y)
      (fref dd 4)
     )
     y
    )
    (fref dd 5)
  ))
  (setf f (/ fn (* x fd))) (setf g (/ gn (* (* x xx) gd))) (go label70) label50
  (if (>= x 6.0) (go label60)) (setf xx (* x x)) (setf y (/ 1.0 (* xx xx)))
  (setf f
   (+
    (*
     (+
      (*
       (+
        (*
         (+ (* (+ (* (+ (* (fref fp 1) y) (fref fp 2)) y) (fref fp 3)) y)
          (fref fp 4)
         )
         y
        )
        (fref fp 5)
       )
       y
      )
      (fref fp 6)
     )
     y
    )
    (fref fp 7)
  ))
  (setf g
   (+
    (*
     (+
      (*
       (+
        (*
         (+ (* (+ (* (+ (* (fref gp 1) y) (fref gp 2)) y) (fref gp 3)) y)
          (fref gp 4)
         )
         y
        )
        (fref gp 5)
       )
       y
      )
      (fref gp 6)
     )
     y
    )
    (fref gp 7)
  ))
  (setf f (/ f x)) (setf g (/ g (* x xx))) (go label70) label60
  (if (>= x (float max)) (go label100)) (setf pix (* pi_ x))
  (setf pixx (* pix x)) (setf y (/ 1.0 pixx)) (setf y (* y y))
  (setf f
   (+
    (*
     (+
      (*
       (+ (* (+ (* (+ (* (fref p 1) y) (fref p 2)) y) (fref p 3)) y) (fref p 4)
       )
       y
      )
      (fref p 5)
     )
     y
    )
    (fref p 6)
  ))
  (setf g
   (+
    (*
     (+
      (*
       (+ (* (+ (* (+ (* (fref q 1) y) (fref q 2)) y) (fref q 3)) y) (fref q 4)
       )
       y
      )
      (fref q 5)
     )
     y
    )
    (fref q 6)
  ))
  (setf f (/ f pix)) (setf g (/ g (* pix pixx))) label70 (setf m x)
  (setf l (mod m 2)) (setf n (+ m (- l))) (setf y (+ x (- m)))
  (setf r (+ x (- n))) (setf y (* y n)) (setf m y) (setf y (+ y (- m)))
  (if (/= (mod m 2) 0) (setf y (+ (+ y (- 0.5)) (- 0.5))))
  (setf y (+ y (* (* 0.5 r) r))) label80 (setf sy (sin1 y)) (setf cy (cos1 y))
  (setf c (+ 0.5 (+ (* f sy) (* (* -1 g) cy))))
  (setf s (+ 0.5 (- (+ (* f cy) (* g sy))))) (if (>= t_ 0.0) (go end_label))
  (setf c (- c)) (setf s (- s)) (go end_label) label100
  (if (< t_ 0.0) (go label110)) (setf c 0.5) (setf s 0.5) (go end_label)
  label110 (setf c (- 0.5)) (setf s (- 0.5)) (go end_label) end_label
  (return (values t c s))
))

