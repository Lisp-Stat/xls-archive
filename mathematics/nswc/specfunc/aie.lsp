(defun aie (x) (declare (type double-float x))
 (prog
  ((aie 0.0d0) (rn0 0.0d0) (rd5 0.0d0) (rn4 0.0d0) (qd5 0.0d0) (qn5 0.0d0)
   (pd5 0.0d0) (pn4 0.0d0) (w 0.0d0) (t_ 0.0d0) (rtx 0.0d0) (bd5 0.0d0)
   (bn7 0.0d0) (ad3 0.0d0) (an9 0.0d0) (r 0.0d0)
  )
  (declare (type real aie)) (declare (type double-float rn0))
  (declare (type double-float rd5)) (declare (type double-float rn4))
  (declare (type double-float qd5)) (declare (type double-float qn5))
  (declare (type double-float pd5)) (declare (type double-float pn4))
  (declare (type double-float w)) (declare (type double-float t_))
  (declare (type double-float rtx)) (declare (type double-float bd5))
  (declare (type double-float bn7)) (declare (type double-float ad3))
  (declare (type double-float an9)) (declare (type double-float r))
  (setq x0 1.587401) (setq an9 7.890029E-6) (setq an8 (- 2.7016547E-5))
  (setq an7 (- 1.17114825E-4)) (setq an6 0.0011417604)
  (setq an5 (- 0.0020564862)) (setq an4 (- 0.009670177)) (setq an3 0.049195256)
  (setq an2 (- 0.038373597)) (setq an1 (- 0.18739492)) (setq an0 0.35502806)
  (setq ad3 2.3088744E-5) (setq ad2 0.038576253) (setq ad1 0.20117985)
  (setq ad0 1.0) (setq bn7 (- 2.5776693E-5)) (setq bn6 3.5051862E-4)
  (setq bn5 (- 0.0014786837)) (setq bn4 (- 0.0015296993))
  (setq bn3 0.029770534) (setq bn2 (- 0.060221605)) (setq bn1 (- 0.09971693))
  (setq bn0 0.35502806) (setq bd5 4.2332697E-4) (setq bd4 0.004859227)
  (setq bd3 0.031696454) (setq bd2 0.15707454) (setq bd1 0.44814056)
  (setq bd0 1.0) (setq pn4 7.5049076E-7) (setq pn3 1.471167E-4)
  (setq pn2 0.0063064457) (setq pn1 0.080786854) (setq pn0 0.2820944)
  (setq pd5 1.232478E-9) (setq pd4 3.844612E-6) (setq pd3 6.1235396E-4)
  (setq pd2 0.023937685) (setq pd1 0.2928903) (setq pd0 1.0)
  (setq qn5 7.097337E-8) (setq qn4 2.4186285E-5) (setq qn3 0.0013819091)
  (setq qn2 0.024187641) (setq qn1 0.14958583) (setq qn0 0.28209478)
  (setq qd5 4.0883853E-7) (setq qd4 1.0381274E-4) (setq qd3 0.005333687)
  (setq qd2 0.08891126) (setq qd1 0.53677833) (setq qd0 1.0)
  (setq rn4 5.173988E-5) (setq rn3 0.0030659556) (setq rn2 0.04366605)
  (setq rn1 0.20373197) (setq rn0 0.2820948) (setq rd5 2.3270717E-7)
  (setq rd4 2.259739E-4) (setq rd3 0.011698527) (setq rd2 0.15921003)
  (setq rd1 0.72872144) (setq rd0 1.0) (if (>= x (- 1.0)) (go label10))
  (multiple-value-setq (dummy_var r phi) (aimp (- x) r phi))
  (setf aie (* r (sin phi))) (go end_label) label10
  (if (>= x 0.0) (go label20))
  (setf aie
   (/
    (+
     (*
      (+
       (*
        (+
         (*
          (+
           (*
            (+
             (* (+ (* (+ (* (+ (* (+ (* an9 x) an8) x) an7) x) an6) x) an5) x)
             an4
            )
            x
           )
           an3
          )
          x
         )
         an2
        )
        x
       )
       an1
      )
      x
     )
     an0
    )
    (+ (* (+ (* (+ (* ad3 x) ad2) x) ad1) x) ad0)
  ))
  (go end_label) label20 (if (>= x 1.0) (go label30))
  (setf aie
   (/
    (+
     (*
      (+
       (*
        (+ (* (+ (* (+ (* (+ (* (+ (* bn7 x) bn6) x) bn5) x) bn4) x) bn3) x)
         bn2
        )
        x
       )
       bn1
      )
      x
     )
     bn0
    )
    (+ (* (+ (* (+ (* (+ (* (+ (* bd5 x) bd4) x) bd3) x) bd2) x) bd1) x) bd0)
  ))
  (if (> x 1.0E-20) (setf aie (* aie (exp (/ (* (* 2.0 x) (sqrt x)) 3.0)))))
  (go end_label) label30 (setf rtx (sqrt x)) (if (> x x0) (go label40))
  (setf t_ (/ 16.0 (* x rtx)))
  (setf w
   (/ (+ (* (+ (* (+ (* (+ (* pn4 t_) pn3) t_) pn2) t_) pn1) t_) pn0)
    (+ (* (+ (* (+ (* (+ (* (+ (* pd5 t_) pd4) t_) pd3) t_) pd2) t_) pd1) t_)
     pd0
  )))
  (setf aie (/ w (sqrt rtx))) (go end_label) label40
  (if (> x 4.0d0) (go label50)) (setf t_ (/ 16.0 (* x rtx)))
  (setf w
   (/
    (+ (* (+ (* (+ (* (+ (* (+ (* qn5 t_) qn4) t_) qn3) t_) qn2) t_) qn1) t_)
     qn0
    )
    (+ (* (+ (* (+ (* (+ (* (+ (* qd5 t_) qd4) t_) qd3) t_) qd2) t_) qd1) t_)
     qd0
  )))
  (setf aie (/ w (sqrt rtx))) (go end_label) label50
  (if (> x 1.0E20) (go label60)) (setf t_ (/ 16.0 (* x rtx)))
  (setf w
   (/ (+ (* (+ (* (+ (* (+ (* rn4 t_) rn3) t_) rn2) t_) rn1) t_) rn0)
    (+ (* (+ (* (+ (* (+ (* (+ (* rd5 t_) rd4) t_) rd3) t_) rd2) t_) rd1) t_)
     rd0
  )))
  (setf aie (/ w (sqrt rtx))) (go end_label) label60
  (setf aie (/ rn0 (sqrt rtx))) (go end_label) end_label (return aie)
))

