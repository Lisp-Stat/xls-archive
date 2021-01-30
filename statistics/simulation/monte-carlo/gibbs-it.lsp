
(defun gibbs-initial (q r s)
(let (
     (ph (normal-quant (* .5 (1+ s))))
     (vv (* q (- 1 q)))
     )
(ceiling (1+ (/ (* vv (^ ph 2)) (^ r 2))))
))

(defun gibbs-iterations  (x q r s &key (delta .001))
(let* (
		(nn (length x))
	 	(ct (quantile x q))
	 	(ix (if-then (<= x ct) (repeat 1 nn) (repeat 0 nn)))
	 	(kk (markov-order ix))
	 	(ab (markov-est ix))
	 	(aa (first ab))
	 	(bb (second ab))
	 	(tp (/ (log (* delta (/ (+ aa bb) (max aa bb))))
	 	    (log (abs (- 1 aa bb)))))
	 	(nb (* kk (ceiling (1+ tp))))
	 	(ph (normal-quant (* .5 (1+ s))))
	 	(rp (/ (* aa bb (- 2 aa bb) (^ ph 2)) (^ (+ aa bb) 3) (^ r 2)))
	 	(np (* kk (ceiling (1+ rp))))
	 	)
(list kk nb np)
))

(defun markov-test (x)
(let (
     (mm (make-array '(2 2 2) :initial-element 0))
     (nn (length x))
     (gg 0)
     )
(dotimes (i (- nn 2))
         (let (
               (i1 (elt x (+ i 2)))
               (i2 (elt x (+ i 1)))
               (i3 (elt x i))
               )
(setf (aref mm i1 i2 i3) (1+ (aref mm i1 i2 i3)))      
))
(dotimes (i 2)
(dotimes (j 2)
(dotimes (k 2)
(let* (
      (ob (aref mm i j k))
      (ff (/ (* (+ (aref mm i j 0) (aref mm i j 1)) 
                (+ (aref mm 0 j k) (aref mm 1 j k)))
             (+ (aref mm 0 j 0) (aref mm 0 j 1)
                (aref mm 1 j 0) (aref mm 1 j 1))))
      (oc (if (= 0.0 ob) 0.0 (log (/ ob ff))))
      )
(setf gg (+ gg (* 2 ob oc))))
)))
(- gg (* 2 (log (- nn 2))))
))

(defun markov-est (x)
(let (
      (nn (length x))
      (mm (make-array '(2 2) :initial-element 0))
      )
(dotimes (i (1- nn))
(setf (aref mm (elt x i) (elt x (1+ i)))
      (1+ (aref mm (elt x i) (elt x (1+ i))))))
(list (/ (aref mm 0 1) (+ (aref mm 0 0) (aref mm 0 1)))
      (/ (aref mm 1 0) (+ (aref mm 1 0) (aref mm 1 1))))

))

(defun markov-order (x)
(let (
      (k 1)
      )
(loop 
 (let (
      (bic (markov-test (thin-sequence x k)))
      )
   (if (> 0.0 bic) (setf k (1+ k)) (return k))))
))

(defun thin-sequence (x k)
(select x (* k (iseq (ceiling (/ (length x) k)))))
)

(defun icdf-rand (icdf n)
(funcall icdf (uniform-rand n)))

(defun cdf-rand (cdf)
(solver cdf (uniform-rand 1)))
