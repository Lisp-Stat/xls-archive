(require "f2cl_macros")

(defun mnbrak
 (ax bx cx fa fb fc func &key (gold 1.618034) (glimit 100.0) (tiny 1.0E-20))
 (declare (type double-float ax)) (declare (type double-float bx))
 (declare (type double-float cx)) (declare (type double-float fa))
 (declare (type double-float fb)) (declare (type double-float fc))
 (declare (type double-float gold)) (declare (type double-float glimit))
 (declare (type double-float tiny))
 (prog ((fu 0.0d0) (ulim 0.0d0) (u 0.0d0) (q 0.0d0) (r 0.0d0) (dum 0.0d0))
  (declare (type double-float fu)) (declare (type double-float ulim))
  (declare (type double-float u)) (declare (type double-float q))
  (declare (type double-float r)) (declare (type double-float dum))
  (setf fa (funcall func ax)) (setf fb (funcall func bx))
  (cond
   ((> fb fa) (setf dum ax) (setf ax bx) (setf bx dum) (setf dum fb)
    (setf fb fa) (setf fa dum)
  ))
  (setf cx (+ bx (* gold (+ bx (- ax))))) (setf fc (funcall func cx)) label1
  (cond
   ((>= fb fc) (setf r (* (+ bx (- ax)) (+ fb (- fc))))
    (setf q (* (+ bx (- cx)) (+ fb (- fa))))
    (setf u
     (+ bx
      (/ (* -1 (+ (* (+ bx (- cx)) q) (* (* -1 (+ bx (- ax))) r)))
       (* 2.0 (sign (max (abs (+ q (- r))) tiny) (+ q (- r))))
    )))
    (setf ulim (+ bx (* glimit (+ cx (- bx)))))
    (cond
     ((> (* (+ bx (- u)) (+ u (- cx))) 0.0) (setf fu (funcall func u))
      (cond
       ((< fu fc) (setf ax bx) (setf fa fb) (setf bx u) (setf fb fu)
        (go label1)
       )
       ((> fu fb) (setf cx u) (setf fc fu) (go label1))
      )
      (setf u (+ cx (* gold (+ cx (- bx))))) (setf fu (funcall func u))
     )
     ((> (* (+ cx (- u)) (+ u (- ulim))) 0.0) (setf fu (funcall func u))
      (cond
       ((< fu fc) (setf bx cx) (setf cx u)
        (setf u (+ cx (* gold (+ cx (- bx))))) (setf fb fc) (setf fc fu)
        (setf fu (funcall func u))
     )))
     ((>= (* (+ u (- ulim)) (+ ulim (- cx))) 0.0) (setf u ulim)
      (setf fu (funcall func u))
     )
     (t (setf u (+ cx (* gold (+ cx (- bx))))) (setf fu (funcall func u)))
    )
    (setf ax bx) (setf bx cx) (setf cx u) (setf fa fb) (setf fb fc)
    (setf fc fu) (go label1)
  ))
  (return (values ax bx cx fa fb fc func))
))

