(defun parea (x y nb) (declare (type (simple-array float (*)) x))
 (declare (type (simple-array float (*)) y)) (declare (type fixnum nb))
 (prog ((parea 0.0d0) (i 0) (a 0.0d0) (nm1 0) (n 0))
  (declare (type real parea)) (declare (type fixnum i))
  (declare (type double-float a)) (declare (type fixnum nm1))
  (declare (type fixnum n)) (setf n nb)
  (if (and (= (fref x 1) (fref x n)) (= (fref y 1) (fref y n)))
   (setf n (+ n (- 1)))
  )
  (arithmetic-if (+ n (- 3)) (go label10) (go label20) (go label30)) label10
  (setf parea 0.0) (go end_label) label20
  (setf parea
   (* 0.5
    (+ (* (+ (fref x 2) (- (fref x 1))) (+ (fref y 3) (- (fref y 1))))
     (* (* -1 (+ (fref x 3) (- (fref x 1)))) (+ (fref y 2) (- (fref y 1))))
  )))
  (go end_label) label30 (setf nm1 (+ n (- 1)))
  (setf a
   (+ (* (fref x 1) (+ (fref y 2) (- (fref y n))))
    (* (fref x n) (+ (fref y 1) (- (fref y nm1))))
  ))
  (fdo ((i 2 (+ i 1))) ((> i nm1) nil)
   (tagbody
    (setf a (+ a (* (fref x i) (+ (fref y (+ i 1)) (- (fref y (+ i (- 1))))))))
  ))
  (setf parea (* 0.5 a)) (go end_label) end_label (return parea)
))

