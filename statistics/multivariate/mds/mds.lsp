;
; read the basis
;
(def zz (list (list '(0 1 0 0) '(0 0 0 0))
              (list '(0 0 1 0) '(0 0 0 0))
			  (list '(0 0 0 1) '(0 0 0 0))
			  (list '(0 0 0 0) '(0 0 1 0))
			  (list '(0 0 0 0) '(0 0 0 1))))
;
; compute number of parameters, number of dimensions, number of objects
;
(def np (length zz))
(def nd (length (elt zz 0)))
(def no (length (elt (elt zz 0) 0)))
;
; read or compute the dissimilarities, weights, precision, bound
;
(def nr (/ (* no (- no 1)) 2))
(def dl (repeat (/ (sqrt nr)) nr))
(def ww (repeat 1 nr))
(def eps .000001)
(def imx 100)
;
(defun make-c (i j)
(let (
     (dg (make-array (list nr nd) :initial-element 0))
     )
(do (k np)
    (
	(setf za (elt zz k))
	(setf df (- (select za (iseq nd) i) (select za (iseq nd) j)))
	(setf (select dg k) df)
	)
(matmult dg (transpose dg))
))
;  
(defun stress (x y)
(sum (^ (- pr (dist x y)) 2))
)
;
(defun smacof (x y)
(loop
(setf bb (bmat x y))
(print (setf xp (matmult bb (vector x y))))
(if (> eps (max (abs (- xp (vector x y))))) 
    (return (+ (* x za) (* y zb))))
(setf x (elt xp 0))
(setf y (elt xp 1))
(print (stress x y))
))

(defun steffenson (x y)
(loop
(dotimes (i 3)
(setf zz (+ (* x za) (* y zb)))
(setf dd (dist zz))
(setf bb (/ dl (+ dd (identity-matrix 4))))
(setf bb (- (diagonal (column-sums bb)) bb))
(setf s11 (sum (diagonal (matmult (transpose za) (matmult bb za)))))
(setf s12 (sum (diagonal (matmult (transpose za) (matmult bb zb)))))
(setf s22 (sum (diagonal (matmult (transpose zb) (matmult bb zb)))))
(setf ss (make-array '(2 2) :initial-contents 
  (list (list s11 s12) (list s12 s22))))
(setf xp (matmult ss (vector x y)))
(setf x (elt xp 0))
(setf y (elt xp 1))
)
(print (stress x y))
))

(defun smaplot (x y)
(plot-points (column-list (smacof x y)))
)

(defun dist (x y)
(let* (
      (z (vector x y))
      (d12 (sqrt (sum (* z (matmult c12 z)))))
      (d13 (sqrt (sum (* z (matmult c13 z)))))
      (d14 (sqrt (sum (* z (matmult c14 z)))))
      (d23 (sqrt (sum (* z (matmult c23 z)))))
      (d24 (sqrt (sum (* z (matmult c24 z)))))
      (d34 (sqrt (sum (* z (matmult c34 z)))))
      )
(list d12 d13 d14 d23 d24 d34)
))

(defun bmat (x y)
(let* (
(dd (dist x y))
(b12 (* (/ pr (elt dd 0)) c12))
(b13 (* (/ pr (elt dd 1)) c13))
(b14 (* (/ pr (elt dd 2)) c14))
(b23 (* (/ pr (elt dd 3)) c23))
(b24 (* (/ pr (elt dd 4)) c24))
(b34 (* (/ pr (elt dd 5)) c34))
)
(+ b12 b13 b14 b23 b24 b34)
))

(defun plot-me ()
(spin-function (function stress) -2 2 -2 2 :num-points 11)
(contour-function (function stress) -2 2 -2 2 :num-points 11)
)

(defun column-sums (x)
 (mapcar #'sum (column-list x)))
