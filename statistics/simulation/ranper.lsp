(defun random-permutation (n)
  (let ((s (iseq n)))
    (dotimes (i n s)
      (let ((k (if (= i 0) 0 (random i)))
            (swap s i k))
      ))
   ))

(defmacro swap (x i j)
  `(let ((z (elt ,x ,i)))
     (aset ,x ,i (elt ,x ,j))
     (aset ,x ,j z),x
     ))


(defmacro aset (x i y)
  `(setf (elt ,x ,i) ,y)
  )