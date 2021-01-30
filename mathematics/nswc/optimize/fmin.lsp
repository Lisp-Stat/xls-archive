(defun fmin (f a0 b0 x w aerr rerr error ind) (declare (type float a0))
 (declare (type float b0)) (declare (type float x)) (declare (type float w))
 (declare (type float aerr)) (declare (type float rerr))
 (declare (type float error)) (declare (type fixnum ind))
 (prog
  ((spmpar 0.0d0) (a 0.0d0) (b 0.0d0) (c1 0.0d0) (c2 0.0d0) (e 0.0d0)
   (fu 0.0d0) (fv 0.0d0) (u 0.0d0) (v 0.0d0) (eps 0.0d0) (eps0 0.0d0)
   (atol 0.0d0) (ftol 0.0d0) (rtol 0.0d0) (tol 0.0d0) (f 0.0d0)
  )
  (declare (type float spmpar)) (declare (type float a))
  (declare (type float b)) (declare (type float c1)) (declare (type float c2))
  (declare (type float e)) (declare (type float fu)) (declare (type float fv))
  (declare (type float u)) (declare (type float v)) (declare (type float eps))
  (declare (type float eps0)) (declare (type float atol))
  (declare (type float ftol)) (declare (type float rtol))
  (declare (type float tol)) (declare (type float f)) (setq eps0 0.005)
  (setq c1 0.38196602) (setq c2 0.618034) (setf eps (spmpar 1)) (setf a a0)
  (setf b b0) (setf ind 0) (setf atol (amax1 aerr 1.0E-20))
  (setf ftol (amax1 (* 2.0 eps) rerr)) (setf rtol (amax1 (* 7.0 eps) rerr))
  (setf e (+ b (- a))) (setf u (+ a (* c1 e))) (setf v (+ a (* c2 e)))
  (setf fu (funcall (f u))) (setf fv (funcall (f v))) label10
  (if (<= e (* eps0 (+ 1.0 (abs a)))) (go label40))
  (arithmetic-if (+ fu (- fv)) (go label20) (go label11) (go label30)) label11
  (if (> fu (funcall (f b))) (go label30)) label20 (setf b v)
  (setf e (+ b (- a))) (setf v u) (setf u (+ a (* c1 e))) (setf fv fu)
  (setf fu (funcall (f u))) (go label10) label30 (setf a u)
  (setf e (+ b (- a))) (setf u v) (setf v (+ a (* c2 e))) (setf fu fv)
  (setf fv (funcall (f v))) (go label10) label40
  (if (or (> a 0.0) (< b 0.0)) (go label41)) (setf w (funcall (f 0.0)))
  (if (<= w (amin1 fu fv)) (go label100)) label41 (if (/= a a0) (go label42))
  (if (= a 0.0) (go label201)) (setf w (funcall (f a)))
  (if (<= w (amin1 fu fv)) (go label130)) (go label201) label42
  (if (/= b b0) (go label201)) (if (= b 0.0) (go label201))
  (setf w (funcall (f b))) (if (<= w (amin1 fu fv)) (go label150))
  (go label201) label100 (if (<= b atol) (go label110)) (setf x (* 0.01 b))
  (if (> w (funcall (f x))) (go label180)) (setf b x) (go label100) label110
  (if (<= (abs a) atol) (go label120)) (setf x (* 0.01 a))
  (if (> w (funcall (f x))) (go label180)) (setf a x) (go label110) label120
  (setf x 0.0) (setf error (amax1 (abs a) b)) (go end_label) label130
  (setf tol (amax1 (* rtol (abs a)) atol)) label131 (setf x (+ a (* 0.01 e)))
  (if (> w (funcall (f x))) (go label180)) (setf b x) (setf e (+ b (- a)))
  (if (> e tol) (go label131)) (setf x a) (setf error e) (go end_label)
  label150 (setf tol (amax1 (* rtol (abs b)) atol)) label151
  (setf x (+ b (* (* -1 0.01) e))) (if (> w (funcall (f x))) (go label180))
  (setf a x) (setf e (+ b (- a))) (if (> e tol) (go label151)) (setf x b)
  (setf error e) (go end_label) label180 (setf e (+ b (- a)))
  (setf u (+ a (* c1 e))) (setf v (+ a (* c2 e))) (setf fu (funcall (f u)))
  (setf fv (funcall (f v))) label200 (setf ind 0) label201
  (if (> fu fv) (go label210)) (setf b v) (setf e (+ b (- a))) (setf v u)
  (setf u (+ a (* c1 e))) (setf fv fu) (setf fu (funcall (f u))) (go label220)
  label210 (setf a u) (setf e (+ b (- a))) (setf u v) (setf v (+ a (* c2 e)))
  (setf fu fv) (setf fv (funcall (f v))) label220
  (if (<= e (amax1 (* rtol (abs a)) atol)) (go label240))
  (if (> (abs (+ fv (- fu))) (* ftol (amax1 (abs fu) (abs fv)))) (go label200))
  (if (= ind 1) (go label241)) (setf ind 1) (go label201) label240 (setf ind 0)
  label241
  (arithmetic-if (+ fu (- fv)) (go label242) (go label243) (go label244))
  label242 (setf x u) (setf w fu) (setf error (* c1 e)) (go end_label) label243
  (setf x v) (setf w fv) (setf error e) (go end_label) label244 (setf x v)
  (setf w fv) (setf error (* c1 e)) (go end_label) end_label
  (return (values f a0 b0 x w aerr rerr error ind))
))

