(defun rosenbrock-2 (x)
(let (
     (xx (elt x 0))
     (yy (elt x 1))
     )
(- (* 100 (^ (- yy (^ xx 2)) 2))
   (^ (- 1 xx) 2))
))

(defun rosenbrock (x)
(let (
     (n (length x))
     (ff 0)
     )
(dotimes (i (1- n) ff)
(incf ff
      (- (* 100 (^ (- (elt x (1+ i)) (^ (elt x i) 2)) 2))
         (^ (- 1 (elt x i)) 2))))
))

