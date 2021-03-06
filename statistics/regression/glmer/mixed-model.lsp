(def x (list (repeat 1 20) (iseq 20)))
(def y (normal-rand 20))
(def z (list (normal-rand 20) (normal-rand 20)))

(defun mixed-model (x y z)
     (let* (
           (nobj (length y))
           (nfix (length x))
           (nran (length z))
           (ssig 1)
           (omeg (repeat 1 nran))
           )
(loop
(setf x0 (copy-list x))
(dotimes (i nfix)
           (setf (select x0 i) 
           (append (select (/ x0 (sqrt ssig)) i) (repeat 0 nran))))
(setf y0 (copy-list y))
(setf y0 (append (/ y0 (sqrt ssig)) (repeat 0 nran)))
(setf z0 (copy-list z))
(dotimes (i nran)
           (setf (select z0 i) 
           (append (select (/ z0 (sqrt ssig)) i) 
           (/ (elem i (iseq nran)) (sqrt (select omeg i))))))
(setf meloen (regression-model (append x0 z0) y0 :intercept nil :print nil))
(setf rsum (send meloen :residual-sum-of-squares))
(setf coef (send meloen :coef-estimates))
(setf rcoe (select coef (+ nfix (iseq nran))))
(setf zar (make-array (list nobj nran) :initial-contents z))
(setf tt 
  (diagonal (inverse (+ (/ (cross-product zar) ssig) 
  (inverse (diagonal omeg))))))
(dotimes (i nran)
  (setf (select omeg i) (/ (^ (select rcoe i) 2) (- 1 (select tt i)))))
(setf ssig (/ rsum (+ nobj (sum tt) (- nran))))
(print coef)
(print omeg)
(print ssig)
)))

(defun elem (x y)
(let* (
      (n (length y))
      (m (iseq n))
      (z (repeat 0 n)))
(dotimes (i n)
(if (= x (select y i))
    (setf (select z i) 1)))
z))

