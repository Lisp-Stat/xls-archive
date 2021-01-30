
(defmeth spin-proto :pca ()
(let* ((n (send self :num-points))
       (x (send self :point-coordinate 0 (iseq n)))
       (y (send self :point-coordinate 1 (iseq n)))
       (z (send self :point-coordinate 2 (iseq n)))
       (c (* (1- n) (covariance-matrix x y z)))
       (m (list (mean x) (mean y) (mean z)))
       (g (eigen c))
       (e (second g))
       (f (sqrt (first g))))
(send self :dircos m (first e) (/ (elt  f 0) 2))
(send self :dircos m (second e) (/ (elt f 1) 2))
(send self :dircos m (third e) (/ (elt f 2) 2))
(print e)
(print f)
))

(defmeth spin-proto :dircos (m w u)
	(send self :abline (+ m (* u w)) (- m (* u w)) )
)

(defmeth spin-proto :abline (a b) 
 (send self :add-lines (make-pairs a b))
)

(defun make-pairs (x y)
(let ((n (length x)))
	(mapcar #'(lambda (z) (list (elt x z) (elt y z))) (iseq n))
))

(def data (list (* 3 (normal-rand 100)) (* 2 (normal-rand 100)) (normal-rand 
100)))

(setf sp (spin-plot data))

(send sp :pca)