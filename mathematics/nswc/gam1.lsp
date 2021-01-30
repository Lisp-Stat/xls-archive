(defun gam1 (a) (declare (type double-float a))
 (prog
  ((p (make-array '(7) :element-type 'float))
   (q (make-array '(5) :element-type 'float))
   (r (make-array '(9) :element-type 'float)) (gam1 0.0d0) (s2 0.0d0) (w 0.0d0)
   (bot 0.0d0) (top 0.0d0) (d 0.0d0) (t_ 0.0d0)
  )
  (declare (type (simple-array float (*)) p))
  (declare (type (simple-array float (*)) q))
  (declare (type (simple-array float (*)) r)) (declare (type real gam1))
  (declare (type double-float s2)) (declare (type double-float w))
  (declare (type double-float bot)) (declare (type double-float top))
  (declare (type double-float d)) (declare (type double-float t_))
  (replace p '(5.8959745E-4) :end 0) (replace p '((- 0.005148898)) :end 0)
  (replace p '(0.0076696817) :end 0) (replace p '(0.059727535) :end 0)
  (replace p '((- 0.23097537)) :end 0) (replace p '((- 0.40907818)) :end 0)
  (replace p '(0.5772157) :end 0) (replace q '(0.004232443) :end 0)
  (replace q '(0.026113203) :end 0) (replace q '(0.15845168) :end 0)
  (replace q '(0.42756963) :end 0) (replace q '(1.0) :end 0)
  (replace r '((- 1.326749E-4)) :end 0) (replace r '(2.66506E-4) :end 0)
  (replace r '(0.0022304766) :end 0) (replace r '((- 0.0118291)) :end 0)
  (replace r '(9.303573E-4) :end 0) (replace r '(0.11837899) :end 0)
  (replace r '((- 0.24475777)) :end 0) (replace r '((- 0.77133036)) :end 0)
  (replace r '((- 0.42278433)) :end 0) (setq s2 0.055939823)
  (setq s1 0.27307615) (setf t_ a) (setf d (+ a (- 0.5)))
  (if (> d 0.0) (setf t_ (+ d (- 0.5))))
  (arithmetic-if t_ (go label30) (go label10) (go label20)) label10
  (setf gam1 0.0) (go end_label) label20
  (setf top
   (+
    (*
     (+
      (*
       (+
        (*
         (+ (* (+ (* (+ (* (fref p 7) t_) (fref p 6)) t_) (fref p 5)) t_)
          (fref p 4)
         )
         t_
        )
        (fref p 3)
       )
       t_
      )
      (fref p 2)
     )
     t_
    )
    (fref p 1)
  ))
  (setf bot
   (+
    (*
     (+ (* (+ (* (+ (* (fref q 5) t_) (fref q 4)) t_) (fref q 3)) t_)
      (fref q 2)
     )
     t_
    )
    1.0
  ))
  (setf w (/ top bot)) (if (> d 0.0) (go label21)) (setf gam1 (* a w))
  (go end_label) label21 (setf gam1 (* (/ t_ a) (+ (+ w (- 0.5)) (- 0.5))))
  (go end_label) label30
  (setf top
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
             (+ (* (+ (* (+ (* (fref r 9) t_) (fref r 8)) t_) (fref r 7)) t_)
              (fref r 6)
             )
             t_
            )
            (fref r 5)
           )
           t_
          )
          (fref r 4)
         )
         t_
        )
        (fref r 3)
       )
       t_
      )
      (fref r 2)
     )
     t_
    )
    (fref r 1)
  ))
  (setf bot (+ (* (+ (* s2 t_) s1) t_) 1.0)) (setf w (/ top bot))
  (if (> d 0.0) (go label31)) (setf gam1 (* a (+ (+ w 0.5) 0.5)))
  (go end_label) label31 (setf gam1 (/ (* t_ w) a)) (go end_label) end_label
  (return gam1)
))

