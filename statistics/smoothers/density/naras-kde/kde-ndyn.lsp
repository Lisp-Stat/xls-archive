(provide "kde-ndyn")
;;;;
;;;; This file contains code that can be used when dynamic loading
;;;; is not available. You lose lotsa speed with this.

(defmeth kde-proto :mean ()
  (let ((data (send self :data))
	(wts (send self :weights)))
    (sum (* data wts))))

(defmeth kde-proto :stddev ()
  (let* ((data (send self :data))
	 (wts (send self :weights))
	 (d (- data (* data wts))))
    (sqrt (sum (* wts (* d d))))))

(defmeth kde-proto :kde ()
  (let ((data (send self :data))
	(wts (send self :weights))
	(np (send self :kernel-points))
	(wid (/ 1 (send self :kernel-bin-width)))
	(kernel (send self :kernel-type))
	(x nil)
	(y nil))
    (setf x (rseq (min data) (max data) np))
    (case kernel
	  ('G (flet ((f (w) 
			(let ((tmp (* (- w data) 4.0 wid)))
			  (* 4 (sum (* (exp (* -0.5 tmp tmp)) wts)) wid 
			     (/ 1 (sqrt (* 2 pi)))))))
		    (setf y (mapcar #'f x))
		    (list x y))) 
	  ('U (flet ((f (w) 
			(let ((tmp (if-else 
				    (< (abs (* (- w data) .75 wid)) .5) 
				    wts 0)))
			  (* 0.666666666667 wid (sum tmp)))))
		    (setf y (mapcar #'f x))
		    (list x y)))
	  ('T (flet ((f (w) 
			(let* ((t1 (* (- w data) wid))
			       (t2 (if-else (> t1 -1) 1 0))
			       (t3 (if-else (< t1 0) 1 0))
			       (t4 (if-else (> (* t2 t3) 0) (1+ t1) 0))
			       (t2 (if-else (>= t1 0) 1 0))
			       (t3 (if-else (< t1 1) 1 0))
			       (t5 (if-else (> (* t2 t3) 0) (- 1 t1) 0)))
			  (* wid (sum (* (+ t4 t5) wts))))))
		    (setf y (mapcar #'f x))
		    (list x y)))
	  ('E (flet ((f (w) 
			(let* ((t1 (* (- w data) 2 wid))
			       (t2 (* wts (if-else (< (abs t1) (sqrt 5))
						   (- 1 (* .2 t1 t1)) 0))))
			  (* (sum t2) 2 wid))))
		    (setf y (mapcar #'f x))
		    (list x y)))
	  (t (flet ((f (w) 
		       (let* ((t1 (* (- w data) 2 wid))
			      (t2 (* wts (if-else 
					  (< (abs t1) 1)
					  (^ (- 1 (* t1 t1)) 2) 0))))
			 (* (sum t2) 0.9375 wid))))
		   (setf y (mapcar #'f x))
		   (list x y))))))
		   

		     

