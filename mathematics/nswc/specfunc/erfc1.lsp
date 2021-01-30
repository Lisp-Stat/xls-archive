(defun erfc1 (ind x) (declare (type fixnum ind))
 (declare (type double-float x))
 (prog
  ((w 0.0d0) (a (make-array '(4) :element-type 'float))
   (b (make-array '(4) :element-type 'float))
   (p (make-array '(8) :element-type 'float))
   (q (make-array '(8) :element-type 'float))
   (r (make-array '(5) :element-type 'float))
   (s (make-array '(5) :element-type 'float)) (erfc1 0.0d0) (c 0.0d0) (e 0.0d0)
   (bot 0.0d0) (top 0.0d0) (t_ 0.0d0) (ax 0.0d0)
  )
  (declare (type double-float w)) (declare (type (simple-array float (*)) a))
  (declare (type (simple-array float (*)) b))
  (declare (type (simple-array float (*)) p))
  (declare (type (simple-array float (*)) q))
  (declare (type (simple-array float (*)) r))
  (declare (type (simple-array float (*)) s)) (declare (type real erfc1))
  (declare (type double-float c)) (declare (type double-float e))
  (declare (type double-float bot)) (declare (type double-float top))
  (declare (type double-float t_)) (declare (type double-float ax))
  (setq c 0.5641896) (replace a '(1.1283792) :end 0)
  (replace a '(0.102201134) :end 0) (replace a '(0.03253241) :end 0)
  (replace a '((- 1.6558183E-4)) :end 0) (replace b '(1.0) :end 0)
  (replace b '(0.42390674) :end 0) (replace b '(0.07013334) :end 0)
  (replace b '(0.0046498897) :end 0) (replace p '(300.45926) :end 0)
  (replace p '(451.91895) :end 0) (replace p '(339.32083) :end 0)
  (replace p '(152.98929) :end 0) (replace p '(43.162228) :end 0)
  (replace p '(7.211758) :end 0) (replace p '(0.5641955) :end 0)
  (replace p '((- 1.3686486E-7)) :end 0) (replace q '(300.45926) :end 0)
  (replace q '(790.9509) :end 0) (replace q '(931.35406) :end 0)
  (replace q '(638.9803) :end 0) (replace q '(277.58545) :end 0)
  (replace q '(77.00015) :end 0) (replace q '(12.782727) :end 0)
  (replace q '(1.0) :end 0) (replace r '(0.2820948) :end 0)
  (replace r '(4.658078) :end 0) (replace r '(21.36882) :end 0)
  (replace r '(26.237015) :end 0) (replace r '(2.1014414) :end 0)
  (replace s '(1.0) :end 0) (replace s '(18.012457) :end 0)
  (replace s '(99.01918) :end 0) (replace s '(187.1148) :end 0)
  (replace s '(94.15378) :end 0) (setf ax (abs x))
  (if (>= ax 0.47) (go label10)) (setf t_ (* x x))
  (setf top
   (+ (* (+ (* (+ (* (fref a 1) t_) (fref a 2)) t_) (fref a 3)) t_) (fref a 4))
  )
  (setf bot
   (+ (* (+ (* (+ (* (fref b 1) t_) (fref b 2)) t_) (fref b 3)) t_) (fref b 4))
  )
  (setf erfc1 (+ 0.5 (+ 0.5 (/ (* (* -1 x) top) bot))))
  (if (/= ind 0) (setf erfc1 (* (exp t_) erfc1))) (go end_label) label10
  (if (> ax 4.0) (go label30))
  (setf top
   (+
    (*
     (+
      (*
       (+
        (*
         (+
          (*
           (+ (* (+ (* (+ (* (fref p 1) ax) (fref p 2)) ax) (fref p 3)) ax)
            (fref p 4)
           )
           ax
          )
          (fref p 5)
         )
         ax
        )
        (fref p 6)
       )
       ax
      )
      (fref p 7)
     )
     ax
    )
    (fref p 8)
  ))
  (setf bot
   (+
    (*
     (+
      (*
       (+
        (*
         (+
          (*
           (+ (* (+ (* (+ (* (fref q 1) ax) (fref q 2)) ax) (fref q 3)) ax)
            (fref q 4)
           )
           ax
          )
          (fref q 5)
         )
         ax
        )
        (fref q 6)
       )
       ax
      )
      (fref q 7)
     )
     ax
    )
    (fref q 8)
  ))
  (setf erfc1 (/ top bot)) label20 (if (= ind 0.0) (go label21))
  (if (< x 0.0) (setf erfc1 (+ (* 2.0 (exp (* x x))) (- erfc1))))
  (go end_label) label21 (setf w (* (dble x) (dble x))) (setf t_ w)
  (setf e (+ w (- (dble t_))))
  (setf erfc1 (* (* (+ 0.5 (+ 0.5 (- e))) (exp (- t_))) erfc1))
  (if (< x 0.0) (setf erfc1 (+ 2.0 (- erfc1)))) (go end_label) label30
  (if (<= x (- 5.5)) (go label40))
  (if (and (= ind 0.0) (> x 50.0)) (go label50)) (setf t_ (expt (/ 1.0 x) 2))
  (setf top
   (+
    (*
     (+ (* (+ (* (+ (* (fref r 1) t_) (fref r 2)) t_) (fref r 3)) t_)
      (fref r 4)
     )
     t_
    )
    (fref r 5)
  ))
  (setf bot
   (+
    (*
     (+ (* (+ (* (+ (* (fref s 1) t_) (fref s 2)) t_) (fref s 3)) t_)
      (fref s 4)
     )
     t_
    )
    (fref s 5)
  ))
  (setf erfc1 (/ (+ c (/ (* (* -1 t_) top) bot)) ax)) (go label20) label40
  (setf erfc1 2.0) (if (/= ind 0) (setf erfc1 (* 2.0 (exp (* x x)))))
  (go end_label) label50 (setf erfc1 0.0) (go end_label) end_label
  (return erfc1)
))

