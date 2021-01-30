;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The subgradient algorithm (explained in detail in N.Z. Shor,
;; Minimization Methods for Non-Differentiable Functions, Springer,
;; 1985, is an easy method to minimize convex but possible
;; non-differentiable functions. This looks rather limited
;; in scope, but it can also be used to solve f_i(x) <= 0
;; if all f_i are convex, and to solve general convex programs
;; (including linear and quadratic programs) by using exact
;; penalty functions (or the Kuhn-Tucker conditions).
;; 
;; Also, of course, it can be used for LAV and Chebyshev regression.
;; Remember, however, that these methods can be very, very slow.
;;
;; Version 1.0 --- Jan de Leeuw --- The very basics. Two step-size
;;                                  methods, the one due to Polyak
;;                                  can be used if we know the value
;;                                  at the minimum (for instance if
;;                                  we solve consistent systems of
;;                                  inequalities.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun subgradient 
  (x function subgradient 
     &key (step-size "simple") (eps 1e-6) (itmax 1000) (f-star 0) (c 1))
  (let* ((x-old (copy-list x))
         (f-old (funcall function x-old))
         x-new
         f-new
         (x-min (copy-list x))
         (f-min f-old)
         g-max
         (i 0))
    (loop
     (let* ((g (funcall subgradient x-old))
            (h (if (string-equal step-size "simple")
                   (simple-step i c)
                   (polyak-step f-old f-star g))))
       (setf x-new (- x-old (* h g)))
       (setf f-new (funcall function x-new))
       (if (< f-new f-min)
           (setf x-min x-new f-min f-new))
       (setf g-max (max (abs g)))
       (format t "itel ~4d f-old ~,10f f-new ~,10f g-max ~,10f~%"
               i f-old f-new g-max)
       (if (or (= i itmax) (> eps g-max))
           (return (list x-min f-min))
           (setf i (1+ i) f-old f-new x-old x-new)))
     )
   )
  )


(defun simple-step (i &key (c 1))
  (/ c (1+ i))
  )

(defun polyak-step (f f-star g &key (gamma 1))
  (/ (* gamma (- f f-star))
     (sum (* g g)))
  )

#|

Example: LAV regression and LMV (least maximum value) regression

(setf x (make-array '(20 4) :displaced-to
                    (coerce (normal-rand 80) 'vector)))

(setf y (+ (mapcar #'sum (row-list x)) (* .1 (normal-rand 20))))

(defun lav-func (b)
  (sum (abs (- y (matmult x b))))
  )

(defun lav-grad (b)
  (- (matmult (signum (- y (matmult x b))) x))
 )

(defun lmv-func (b)
 (max (abs (- y (matmult x b))))
  )

(defun lmv-grad (b)
  (let* ((u (- y (matmult x b)))
         (r (abs u))
         (s (max r))
         (k (position s r)))
    (- (* (signum (elt u k)) (elt (row-list x) k)))
    )
  )

(subgradient '(1 1 1 1) #'lav-func #'lav-grad :step-size "simple")

(subgradient '(1 1 1 1) #'lmv-func #'lmv-grad :step-size "simple")

#|                    

Solving the minimization problem of f(x) over x >= 0, means
solving Df(x) >= 0 and x >= 0 and <x, Df(x)> <= 0, i.e. we
must minimize max[-Df_i(x),-x_i,<x,Df(x)>], and we know the
minimum is equal to zero.

(setf a #2A((3 -1 -1 0)(-1 3 -1 -1)(-1 -1 3 -1)(0 -1 -1 3)))

(defun pos-func (x)
  (let ((dx (coerce (matmult a x) 'list)))
    (max (max (- x)) (max (- dx)) (sum (* x dx)))
    )
  )

(defun pos-grad (x)
  (let* ((g (repeat 0 (length x)))
         (dx (coerce (matmult a x) 'list))
         (m1 (max (- x)))
         (k1 (position m1 (- x)))
         (m2 (max (- dx)))
         (k2 (position m2 (- dx)))
         (m3 (sum (* x dx))))
    (cond 
      ((and (> m1 m2) (> m1 m3))
           (setf (elt g k1) -1))
      ((and (> m2 m1) (> m2 m3))
       (setf g (- (elt (row-list a) k2))))
      (t (setf g (* 2 dx))))
    g
    )
  )

(subgradient '(1 -1 -1 1) #'pos-func #'pos-grad :step-size "simple")

(subgradient '(1 -1 -1 1) #'pos-func #'pos-grad :step-size "polyak")

|#
