(require "f2cl_macros")

(defun brent
 (ax bx cx f tol xmin &key (itmax 100) (cgold 0.381966) (zeps 1.0E-10))
 (declare (type double-float ax)) (declare (type double-float bx))
 (declare (type double-float cx)) (declare (type double-float tol))
 (declare (type double-float xmin)) (declare (type fixnum itmax))
 (declare (type double-float cgold)) (declare (type double-float zeps))
 (prog
  ((xbrent 0.0d0) (fu 0.0d0) (u 0.0d0) (d 0.0d0) (etemp 0.0d0) (p 0.0d0)
   (q 0.0d0) (r 0.0d0) (tol2 0.0d0) (tol1 0.0d0) (xm 0.0d0) (iter 0) (fw 0.0d0)
   (fv 0.0d0) (fx 0.0d0) (e 0.0d0) (x 0.0d0) (w 0.0d0) (v 0.0d0) (b 0.0d0)
   (a 0.0d0)
  )
  (declare (type double-float xbrent)) (declare (type double-float fu))
  (declare (type double-float u)) (declare (type double-float d))
  (declare (type double-float etemp)) (declare (type double-float p))
  (declare (type double-float q)) (declare (type double-float r))
  (declare (type double-float tol2)) (declare (type double-float tol1))
  (declare (type double-float xm)) (declare (type fixnum iter))
  (declare (type double-float fw)) (declare (type double-float fv))
  (declare (type double-float fx)) (declare (type double-float e))
  (declare (type double-float x)) (declare (type double-float w))
  (declare (type double-float v)) (declare (type double-float b))
  (declare (type double-float a)) (setf a (min ax cx)) (setf b (max ax cx))
  (setf v bx) (setf w v) (setf x v) (setf e 0.0) (setf fx (funcall f x))
  (setf fv fx) (setf fw fx)
  (fdo ((iter 1 (+ iter 1))) ((> iter itmax) nil)
   (tagbody (setf xm (* 0.5 (+ a b))) (setf tol1 (+ (* tol (abs x)) zeps))
    (setf tol2 (* 2.0 tol1))
    (if (<= (abs (- x xm)) (- tol2 (* 0.5 (- b a)))) (go label3))
    (cond
     ((> (abs e) tol1) (setf r (* (+ x (- w)) (+ fx (- fv))))
      (setf q (* (+ x (- v)) (+ fx (- fw))))
      (setf p (+ (* (+ x (- v)) q) (* (* -1 (+ x (- w))) r)))
      (setf q (* 2.0 (+ q (- r)))) (if (> q 0.0) (setf p (- p)))
      (setf q (abs q)) (setf etemp e) (setf e d)
      (if
       (or (>= (abs p) (abs (* 0.5 q etemp))) (<= p (* q (- a x)))
           (>= p (* q (- b x))))
       (go label1)
      )
      (setf d (/ p q)) (setf u (+ x d))
      (if (or (< (+ u (- a)) tol2) (< (+ b (- u)) tol2))
       (setf d (sign tol1 (+ xm (- x))))
      )
      (go label2)
    ))
    label1 (cond ((>= x xm) (setf e (+ a (- x)))) (t (setf e (+ b (- x)))))
    (setf d (* cgold e)) label2
    (cond ((>= (abs d) tol1) (setf u (+ x d))) (t (setf u (+ x (sign tol1 d))))
    )
    (setf fu (funcall f u))
    (cond
     ((<= fu fx) (cond ((>= u x) (setf a x)) (t (setf b x))) (setf v w)
      (setf fv fw) (setf w x) (setf fw fx) (setf x u) (setf fx fu)
     )
     (t (cond ((< u x) (setf a u)) (t (setf b u)))
      (cond
       ((or (<= fu fw) (= w x)) (setf v w) (setf fv fw) (setf w u) (setf fw fu)
       )
       ((or (<= fu fv) (= v x) (= v w)) (setf v u) (setf fv fu))
  )))))
  (error "Brent exceed maximum iterations.") label3 (setf xmin x)
  (setf xbrent fx) (return xbrent)
))

