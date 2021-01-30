
(defun linear (a b)
   (if (zerop a)
       (if (zerop b)
           (error "eq. homogeneous")
           (error "eq. inconsistent"))
       (if (zerop b)
           (list 0)
           (list (/ (- b) a))
       )
    )
)


(defun quadratic (a b c)
   (cond ((minusp a) (quadratic (- a) (- b) (- c)))
         ((zerop a) (linear b c))
         ((zerop c) (cons 0 (linear a b)))
         (t (quadratic-aux a b c (- (* b b) (* 4 a c))))
   )
)

(defun check-quadratic (a b c)
     (dolist (x (quadratic a b c))
        (format t "~%For x = ~a, the quadratic yields ~a."
                x
               (+ (* (+ (* a x) b) x) c)))
(values))



(defun quadratic-aux (a b c discriminant)
   (cond ((minusp discriminant)
          (quadratic-conjugate (/ (- b) (* 2 a))
                               (/ (sqrt (- discriminant)) (* 2 a))))
         ((zerop discriminant)
          (quadratic-equal (/ (- b) (* 2 a))))
         ((minusp b)
          (quadratic-real-p a 
                            (- (sqrt discriminant) b)
                            c))
          (t (quadratic-real-m a
                               (- (+ (sqrt discriminant) b))
                               c)
          )
    )
)


(defun quadratic-equal (x) (list x x))
                               
(defun quadratic-conjugate (real imaginary)
   (list (complex real imaginary)
         (complex real (- imaginary))))
          
(defun quadratic-real-p (a rat c)
   (list (/ rat (* 2 a))
         (/ (* 2 c) rat)))
                               
(defun quadratic-real-m (a rat c)
   (list (/ (* 2 c) rat)
         (/ rat (* 2 a))))
 




(defun cubic (a b c d)
   (cond ((minusp a) (cubic (- a) (- b) (- c) (- d)))
         ((zerop a) (quadratic b c d))
         ((zerop d) (cons 0 ( quadratic a b c)))
         (t (cubic-aux a
                       b
                       (quadratic 1
                                  (+ (* 2 b b b)
                                     (* 9 a (- (* 3 a d) (* b c))))
                                     (expt (- (* b b) (* 3 a c))
                                           3))))))

(defun check-cubic (a b c d)
   (dolist (x (cubic a b c d))
     (format t "~%For x = ~a, the cubic yields ~a."
             x
             (+ (* (+ (* (+ (* a x) b) x) c) x) d)))
(values))

(defun cube-root (y)
  (if (zerop y) 0
      (if (minusp y) (- (cube-root (- y)))
          (cube-root-iter y (expt y (/ 1 3))))))

(defun cube-root-iter (y x) (/ (+ x x (/ y (* x x))) 3))

(defun check-cube-root (x)
  (let ((answer (cube-root x)))
    (format t "~%Cubic root of ~a is ~a and ~a**3 is ~a."
            x answer answer (expt answer 3)))
(values))

(defun cubic-aux (a b roots)
  (if (complexp (first roots))
      (cubic-real a
                  b 
                  (abs (first roots))
                  (phase (first roots)))  
      (cubic-conjugate a
                       b
                       (cube-root (first roots))
                       (cube-root (second roots)))))


(defun cubic-conjugate (a b r s)
   (let ((sroot (/ (- (+ r s) b) (* a 3)))   
         (real (/ (- (- (/ (+ r s ) 2)) b) (* a 3))))
      (if (= r s)
          (cubic-conjugate-equal sroot real)
          (let ((imag (/ (* (- r s) (/ (sqrt 3) 2)) (* a 3))))
            (cubic-conjugate-aux sroot real imag)))))
                    
(defun cubic-conjugate-equal (sroot droot)
  (cons sroot (list droot droot)))

                        
(defun cubic-conjugate-aux (real-root real imag)
   (list real-root
          (complex real imag)
          (complex real (- imag))))

(defun cubic-real (a b rho theta)
   (cubic-real-aux a
                   b
                   (* 2 (cube-root rho))
                   (/ (cos (/ theta 3)) -2)
                   (/ (* (sin (/ theta 3)) (sqrt 3)) 2)))
                    
(defun cubic-real-aux (a b rd cd sd)
   (list (/ (- (* -2 rd cd) b) (* 3 a))
         (/ (- (* rd (+ cd sd)) b) (* 3 a))
         (/ (- (* rd (- cd sd)) b) (* 3 a))))






(defun quartic (a b c d e)
   (cond ((minusp a) (quartic (- a) (- b) (- c) (- d) (- e)))
         ((zerop a) (cubic b c d e))
         ((zerop e) (cons 0 (cubic a b c d)))
         (t (quartic-aux a
                         b
                         c
                         d
                         e
                         (first (cubic 1
                                       (- c)
                                       (- (* b d) (* 4 a e))
                                       (- (* 4 a c e)
                                          (+ (* a d d)
                                             (* b b e)))))))))


(defun quartic-aux (a b c d e s)
   (quartic-split a
                  b
                  (sqrt (- (* b b) (* 4 a (- c s))))
                  s
                  (sqrt (- (* s s) (* 4 a e)))
                  (- (* b s) (* 2 a d))))

(defun quartic-split (a b r1 s r2 bs-2ad)
  (if (minusp (* r1 r2 bs-2ad))
      (append (quadratic (* 2 a) (- b r1) (+ s r2))
              (quadratic (* 2 a) (+ b r1) (- s r2)))
      (append (quadratic (* 2 a) (- b r1) (- s r2))
              (quadratic (* 2 a) (+ b r1) (+ s r2)))))

(defun check-quartic (a b c d e)
  (dolist (x (quartic a b c d e))
     (format t "~%For x = ~a, the quartic yields ~a."
             x
             (+ (* (+ (* (+ (* (+ (* a x) b) x) c) x) d) x) e)))
(values))



