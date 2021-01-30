;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; comb-generator - generate combinations in lexicographic order
;
; n         - number of objects from which to choose
; m         - number of objects chosen
; last-comb - t, if no combinations have been generated and m>=0.
;             nil, if no more combinations can be generated.
;             the last combination generated, otherwise
; mark      - a list indicating which digits were used in the last
;             combination generated.  t indicates a digit was not
;             used, nil indicates a digit was used
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto comb-generator '(n m last-comb mark))

(defmeth comb-generator :isnew (n m)
  (cond ((and (integerp n) (integerp m) (>= n m) (>= n 0) (>= m 0))
         (send self :slot-value 'n n)
         (send self :slot-value 'm m)
         (cond ((= m 0)
                (send self :slot-value 'last-comb nil))
               (t
                (send self :slot-value 'last-comb t)))
         (send self :slot-value 'mark nil))
        (t
         (format *standard-output* "Usage:~%")
         (format *standard-output* "~%")
         (format *standard-output* "    (send comb-generator :new n m)~%")
         (format *standard-output* "~%")
         (format *standard-output* "where n>=m>=0 are integers~%"))))

(defmeth comb-generator :next-comb ()
  (let ((last-comb (send self :slot-value 'last-comb))
        (mark (send self :slot-value 'mark))
        (m (send self :slot-value 'm))
        (n (send self :slot-value 'n)))
    (cond ((eq last-comb t)
	   (send self :slot-value 'mark
		 (apply #'vector
			(append (repeat nil (+ m 1)) (repeat t (- n m)))))
	   (send self :slot-value 'last-comb
                 (apply #'vector (iseq 1 (send self :slot-value 'm)))))
	  ((eq last-comb nil)
           nil)
	  (t
	   (cond ((do ((i (- m 1) (- i 1)))
		      ((and (eq i 0) (eq (aref last-comb 0) (- n m -1))) nil)
		      (setf (aref mark (aref last-comb i)) t)
		      (cond ((do ((j (+ (aref last-comb i) 1) (+ j 1)))
				 ((> j (+ n i 1 (- m))) nil)
				 (cond ((aref mark j)
					(setf (aref mark j) nil)
					(setf (aref last-comb i) j)
					(do ((k (+ i 1) (+ k 1))
					     (l (+ j 1) (+ l 1)))
					    ((> k (- m 1)) t)
					    (setf (aref mark j) nil)
					    (setf (aref last-comb k) l))
					(return t))))
			     (return t))))
		  t)
		 (t
		  (setf last-comb nil)))
	   (send self :slot-value 'mark mark)
	   (send self :slot-value 'last-comb last-comb)))))

