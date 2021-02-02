(defun monor (x)
  "ARGS: x
This takes a list of three sequences of equal length. It then
computes the monotone regression on the first sequence, using
weights in the second sequence. The third sequence in the list X
is a work array indicating size of the blocks in the monotone
regression. In the call this is usually just a vector of ones,
but it changes in the recursive calls to the function."
  (let* (
	 (values (coerce (elt x 0) 'list))
	 (weights (coerce (elt x 1) 'list))
	 (blocks (coerce (elt x 2) 'list))
	 (n (length values))
	 (last-right (if (= n 1) nil
			 (position t (< (difference values) 0)))))
    (cond
      ((not last-right) (setf result ())
       (dotimes (i n result)
         (setf result
               (append result (repeat (elt values i) (elt blocks i))))))
      (t (let* (
		(first-wrong (1+ last-right))
		(head (iseq last-right))
		(tail (if (= n (1+ first-wrong)) nil
                          (iseq (1+ first-wrong) (1- n))))
		(u (elt values last-right))
		(v (elt values first-wrong))
		(a (elt weights last-right))
		(b (elt weights first-wrong))
		(p (elt blocks last-right))
		(q (elt blocks first-wrong))
		(r (+ a b))
		(h (+ p q))
		(s (/ (+ (* a u) (* b v)) r)))
	   (setf values (append (select values head) (list s) (select values tail)))
	   (setf weights (append (select weights head) (list r) (select weights tail)))
	   (setf blocks (append (select blocks head) (list h) (select blocks tail)))
	   (monor (list values weights blocks)))))))
