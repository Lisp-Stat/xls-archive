(defun erfinv (p q) (declare (type double-float p))
 (declare (type double-float q))
 (prog
  ((a (make-array '(6) :element-type 'float))
   (b (make-array '(6) :element-type 'float))
   (a1 (make-array '(7) :element-type 'float))
   (b1 (make-array '(7) :element-type 'float))
   (a2 (make-array '(9) :element-type 'float))
   (b2 (make-array '(8) :element-type 'float))
   (a3 (make-array '(9) :element-type 'float))
   (b3 (make-array '(6) :element-type 'float)) (erfinv 0.0d0) (v1 0.0d0)
   (s 0.0d0) (v 0.0d0) (t_ 0.0d0) (eps 0.0d0)
  )
  (declare (type (simple-array float (*)) a))
  (declare (type (simple-array float (*)) b))
  (declare (type (simple-array float (*)) a1))
  (declare (type (simple-array float (*)) b1))
  (declare (type (simple-array float (*)) a2))
  (declare (type (simple-array float (*)) b2))
  (declare (type (simple-array float (*)) a3))
  (declare (type (simple-array float (*)) b3)) (declare (type real erfinv))
  (declare (type double-float v1)) (declare (type double-float s))
  (declare (type double-float v)) (declare (type double-float t_))
  (declare (type double-float eps)) (setq c1 0.87890625) (setq c 0.5625)
  (setq c2 (- 230.25851)) (replace a '((- 20.129402)) :end 0)
  (replace a '(276.2427) :end 0) (replace a '((- 969.7933)) :end 0)
  (replace a '(1296.7086) :end 0) (replace a '((- 720.42755)) :end 0)
  (replace a '(140.0217) :end 0) (replace b '((- 62.202057)) :end 0)
  (replace b '(503.37473) :end 0) (replace b '((- 1337.7938)) :end 0)
  (replace b '(1494.9705) :end 0) (replace b '((- 731.23083)) :end 0)
  (replace b '(129.10463) :end 0) (replace a1 '((- 13.490186)) :end 0)
  (replace a1 '(88.05852) :end 0) (replace a1 '((- 145.53644)) :end 0)
  (replace a1 '(93.40783) :end 0) (replace a1 '((- 26.981434)) :end 0)
  (replace a1 '(3.5243742) :end 0) (replace a1 '((- 0.1690478)) :end 0)
  (replace b1 '((- 31.848618)) :end 0) (replace b1 '(125.9118) :end 0)
  (replace b1 '((- 160.43524)) :end 0) (replace b1 '(87.23495) :end 0)
  (replace b1 '((- 22.424852)) :end 0) (replace b1 '(2.6848123) :end 0)
  (replace b1 '((- 0.120322116)) :end 0) (replace a2 '(0.003551096) :end 0)
  (replace a2 '(0.8545922) :end 0) (replace a2 '(2.0479722) :end 0)
  (replace a2 '(2.881692) :end 0) (replace a2 '(3.22838) :end 0)
  (replace a2 '(1.1091677) :end 0) (replace a2 '(0.12149027) :end 0)
  (replace a2 '(0.004097488) :end 0) (replace a2 '(3.1008087E-5) :end 0)
  (replace b2 '(2.162962) :end 0) (replace b2 '(4.119797) :end 0)
  (replace b2 '(4.1402845) :end 0) (replace b2 '(3.432364) :end 0)
  (replace b2 '(1.1186272) :end 0) (replace b2 '(0.12159078) :end 0)
  (replace b2 '(0.0040975288) :end 0) (replace b2 '(3.1008094E-5) :end 0)
  (replace a3 '(0.34219512) :end 0) (replace a3 '((- 0.8343342)) :end 0)
  (replace a3 '(0.67911434) :end 0) (replace a3 '(1.0984219) :end 0)
  (replace a3 '(0.22681436) :end 0) (replace a3 '(0.013705049) :end 0)
  (replace a3 '(2.814223E-4) :end 0) (replace a3 '(1.8994793E-6) :end 0)
  (replace a3 '(3.2054055E-9) :end 0) (replace b3 '(1.1253486) :end 0)
  (replace b3 '(0.22751728) :end 0) (replace b3 '(0.013710923) :end 0)
  (replace b3 '(2.8143497E-4) :end 0) (replace b3 '(1.8994806E-6) :end 0)
  (replace b3 '(3.205405E-9) :end 0)
  (if (or (< p 0.0) (< q 0.0)) (go label100))
  (setf eps (amax1 (spmpar 1) 1.0E-15)) (setf t_ (+ 0.5 (+ 0.5 (- (+ p q)))))
  (if (> (abs t_) (* 2.0 eps)) (go label100)) (if (= q 0.0) (go label50))
  (if (> p 0.75) (go label10)) (setf v (+ (* p p) (- c)))
  (setf t_
   (* p
    (+
     (*
      (+
       (*
        (+ (* (+ (* (+ (* (fref a 6) v) (fref a 5)) v) (fref a 4)) v)
         (fref a 3)
        )
        v
       )
       (fref a 2)
      )
      v
     )
     (fref a 1)
  )))
  (setf s
   (+
    (*
     (+
      (*
       (+ (* (+ (* (+ (* (+ v (fref b 6)) v) (fref b 5)) v) (fref b 4)) v)
        (fref b 3)
       )
       v
      )
      (fref b 2)
     )
     v
    )
    (fref b 1)
  ))
  (go label40) label10 (if (> p 0.9375) (go label20))
  (setf v (+ (* p p) (- c1)))
  (setf t_
   (* p
    (+
     (*
      (+
       (*
        (+
         (*
          (+ (* (+ (* (+ (* (fref a1 7) v) (fref a1 6)) v) (fref a1 5)) v)
           (fref a1 4)
          )
          v
         )
         (fref a1 3)
        )
        v
       )
       (fref a1 2)
      )
      v
     )
     (fref a1 1)
  )))
  (setf s
   (+
    (*
     (+
      (*
       (+
        (*
         (+ (* (+ (* (+ (* (+ v (fref b1 7)) v) (fref b1 6)) v) (fref b1 5)) v)
          (fref b1 4)
         )
         v
        )
        (fref b1 3)
       )
       v
      )
      (fref b1 2)
     )
     v
    )
    (fref b1 1)
  ))
  (go label40) label20 (setf v1 (alog q)) (setf v (/ 1.0 (sqrt (- v1))))
  (if (< v1 c2) (go label30))
  (setf t_
   (+
    (*
     (+
      (*
       (+
        (*
         (+
          (*
           (+
            (*
             (+ (* (+ (* (+ (* (fref a2 9) v) (fref a2 8)) v) (fref a2 7)) v)
              (fref a2 6)
             )
             v
            )
            (fref a2 5)
           )
           v
          )
          (fref a2 4)
         )
         v
        )
        (fref a2 3)
       )
       v
      )
      (fref a2 2)
     )
     v
    )
    (fref a2 1)
  ))
  (setf s
   (* v
    (+
     (*
      (+
       (*
        (+
         (*
          (+
           (*
            (+
             (* (+ (* (+ (* (+ v (fref b2 8)) v) (fref b2 7)) v) (fref b2 6)) v
             )
             (fref b2 5)
            )
            v
           )
           (fref b2 4)
          )
          v
         )
         (fref b2 3)
        )
        v
       )
       (fref b2 2)
      )
      v
     )
     (fref b2 1)
  )))
  (go label40) label30
  (setf t_
   (+
    (*
     (+
      (*
       (+
        (*
         (+
          (*
           (+
            (*
             (+ (* (+ (* (+ (* (fref a3 9) v) (fref a3 8)) v) (fref a3 7)) v)
              (fref a3 6)
             )
             v
            )
            (fref a3 5)
           )
           v
          )
          (fref a3 4)
         )
         v
        )
        (fref a3 3)
       )
       v
      )
      (fref a3 2)
     )
     v
    )
    (fref a3 1)
  ))
  (setf s
   (* v
    (+
     (*
      (+
       (*
        (+ (* (+ (* (+ (* (+ v (fref b3 6)) v) (fref b3 5)) v) (fref b3 4)) v)
         (fref b3 3)
        )
        v
       )
       (fref b3 2)
      )
      v
     )
     (fref b3 1)
  )))
  label40 (setf erfinv (/ t_ s)) (go end_label) label50
  (setf erfinv (spmpar 3)) (go end_label) label100 (setf erfinv (- (spmpar 3)))
  (go end_label) end_label (return erfinv)
))

