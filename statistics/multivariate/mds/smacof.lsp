;
; read the basis
;
(def z5 (list (list '(0 1 0 0) '(0 0 0 0))
              (list '(0 0 1 0) '(0 0 0 0))
              (list '(0 0 0 1) '(0 0 0 0))
              (list '(0 0 0 0) '(0 0 1 0))
              (list '(0 0 0 0) '(0 0 0 1))))

(def z2 (list (list '(0 1 .5 .5) (list 0 0 (sqrt .75) (sqrt (/ 12))))
              (list '(0 0 1 1) '(0 1 0 1)))) 
(def zz z2)
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
(def ctot (make-array (list nr np np) :initial-element 0))
;
(defun make-c (i j)
(let (
     (dg (make-array (list nr nd) :initial-element 0))
     (cc (make-array (list np np) :initial-element 0))
     )
(dotimes (k np)
	(setf za (make-array (list nd no) :initial-contents (select zz k)))
	(setf da (- (select za (iseq nd) i) (select za (iseq nd) j)))
(dotimes (l (+ 1 k))
	(setf zb (make-array (list nd no) :initial-contents (select zz l)))
	(setf db (- (select zb (iseq nd) i) (select zb (iseq nd) j)))
	(setf (aref cc k l) (sum (* da db)))
	))
(- (+ cc (transpose cc)) (diagonal (diagonal cc)))
))
;
(defun make-all-c ()
(setf csum (make-array (list np np) :initial-element 0))
(dotimes (i no)
(dotimes (j i)
         (setf cc (make-c i j))
         (setf k (+ j (/ (* i (- i 1)) 2)))
(dotimes (a np)
(dotimes (b np)
(setf (aref csum a b) (+ (aref csum a b) (aref cc a b)))
))))
(setf kk (make-array (list np np) :initial-contents (eigenvectors csum)))
(setf ll (eigenvalues csum))
(dotimes (i no)
(dotimes (j i)
         (setf cc (matmult (matmult kk (make-c i j)) (transpose kk)))
         (setf cc (/ cc (sqrt (outer-product ll ll))))
         (setf k (+ j (/ (* i (- i 1)) 2)))
(dotimes (a np)
(dotimes (b np)
(setf (aref ctot k a b) (aref cc a b))
))))
)
;
; Now normalize all C
;
(make-all-c)
;
(defun dist (x)
(let (
     (dd (repeat 0 nr))
     (cc (make-array (list np np) :initial-element 0))
     )
(dotimes (i nr)
     (setf cc (make-array (list np np) :displaced-to
           (coerce (select ctot i (iseq np) (iseq np)) 'vector)))
     (setf (nth i dd) 
           (sqrt (sum (* x (matmult cc x)))))
)
dd))
;
(defun update (ww dl x)
(matmult (bmat ww dl x) x)
)
;
(defun stress (ww dl x)
(let (
     (dd (dist x))
     )
(sum (* ww (^ (- dl dd) 2)))
))
;
(defun smacof (ww dl x)
(loop
(print (setf xp (update ww dl x)))
(if (> eps (max (abs (- xp x)))) 
    (return x)
    )
(setf x (copy-list xp))
(print (stress ww dl x))
))
;
(defun bmat (ww dl x)
(let* (
      (dd (dist x))
      (uu (* ww (/ dl dd)))
      (csum (make-array (list np np) :initial-element 0))
      )
(dotimes (i nr)
     (setf cc (make-array (list np np) :displaced-to
           (coerce (select ctot i (iseq np) (iseq np)) 'vector)))
     (setf csum (+ csum (* (nth i uu) cc))))
csum
))
;
(defun hmat (ww dl x)
(let* (
      (dd (dist x))
      (uu (* ww (/ dl (^ dd 3))))
      (csum (make-array (list np np) :initial-element 0))
      )
(dotimes (i nr)
     (setf cc (make-array (list np np) :displaced-to
           (coerce (select ctot i (iseq np) (iseq np)) 'vector)))
     (setf cx (matmult cc x))
     (setf csum (+ csum (* (nth i uu) (outer-product cx cx)))))
(- (bmat ww dl x) csum)
))
;
(defun driver (ww dl x)
(print "solution")
(pprint (setf xx (smacof ww dl x)))
(print "Distances")
(print (dist xx))
(print "B-matrix")
(print-matrix (setf bb (bmat ww dl xx)))
(print "H-matrix")
(print-matrix (setf hh (hmat ww dl xx)))
(pprint (eigenvalues bb))
(pprint (eigenvalues hh))
)
;
(defun newton (ww dl x)
(loop
(print (setf xp 
       (matmult (inverse (- (identity-matrix np) (hmat ww dl x)))
       (update ww dl x))
))
(if (> eps (max (abs (- xp x)))) 
    (return x)
    )
(setf x (copy-list xp))
(print (stress ww dl x))
))
;
(defun invscal (ww dl x)
(let
   (
   (xx (smacof ww dl x))
   (dd (dist xx))
   (nu (make-array (list np nr) :initial-element 0))
   )
(dotimes (i nr)
     (setf cc (make-array (list np np) :displaced-to
           (coerce (select ctot i (iseq np) (iseq np)) 'vector)))
(setf (select nu (iseq np) i)
(make-array (list np 1) :displaced-to
(coerce (* (nth i ww) (/ (nth i dd)) (matmult cc xx)) 'vector)))
)
(sv-decomp (transpose nu))
))
	 
