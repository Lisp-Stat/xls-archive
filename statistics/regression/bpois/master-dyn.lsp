;;;;
;;;; This file provides the code for calculating the importance
;;;; weights when dynamic loading is available.
;;;; The User has to change the program calc_weights.c whenever 
;;;; different priors are used.
;;;; Currently the C program is set up for use with Normal Priors.
;;;;
(provide "master-dyn")

(dolist (x *reweighting-code*)
        (dyn-load x))

(defmeth master-proto :calc-weights ()
  (let ((n (send self :n))
	(p (send self :p))
	(m (send self :how-many-chains))
	(d (send self :data))
	(h-mix (send self :mix-dist))
	(hvals (send self :hyper-vals)))
    (setf (slot-value 'weights)
	  (select (call-cfun "calc_weights" n p m d hvals h-mix
			     (repeat 0.0 (* n m))) 6))))


	  


