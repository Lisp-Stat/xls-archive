(require "f2cl_macros")

(defun balanc (a n np &key (radix 2.0) (sqrdx 4.0))
 (declare (type (simple-array double-float (* *)) a)) (declare (type fixnum n))
 (declare (type fixnum np)) (declare (type double-float radix))
 (declare (type double-float sqrdx))
 (prog ((s 0.0d0) (f 0.0d0) (g 0.0d0) (j 0) (r 0.0d0) (c 0.0d0) (i 0) (last 0))
  (declare (type double-float s)) (declare (type double-float f))
  (declare (type double-float g)) (declare (type fixnum j))
  (declare (type double-float r)) (declare (type double-float c))
  (declare (type fixnum i)) (declare (type fixnum last)) label1 (setf last 1)
  (fdo ((i 1 (+ i 1))) ((> i n) nil)
   (tagbody (setf c 0.0) (setf r 0.0)
    (fdo ((j 1 (+ j 1))) ((> j n) nil)
     (tagbody
      (cond
       ((/= j i) (setf c (+ c (abs (fref a j i))))
        (setf r (+ r (abs (fref a i j))))
    ))))
    (cond
     ((and (/= c 0.0) (/= r 0.0))
      (tagbody (setf g (/ r radix)) (setf f 1.0) (setf s (+ c r)) label2
       (cond ((< c g) (setf f (* f radix)) (setf c (* c sqrdx)) (go label2)))
       (setf g (* r radix)) label3
       (cond ((> c g) (setf f (/ f radix)) (setf c (/ c sqrdx)) (go label3)))
       (cond
        ((< (/ (+ c r) f) (* 0.95 s)) (setf last 0) (setf g (/ 1.0 f))
         (fdo ((j 1 (+ j 1))) ((> j n) nil)
          (tagbody (fset (fref a i j) (* (fref a i j) g)))
         )
         (fdo ((j 1 (+ j 1))) ((> j n) nil)
          (tagbody (fset (fref a j i) (* (fref a j i) f)))
  ))))))))
  (if (= last 0) (go label1)) (return (values a n np))
))

