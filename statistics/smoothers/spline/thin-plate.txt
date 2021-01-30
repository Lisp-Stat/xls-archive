;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Xlisp-Stat has a natural cubic spline interpolation routine, based on
;; the C code in Numerical Recipes, but no natural cubic spline smoother
;; (which the new S has).
;;
;; As explained in the excellent little book by Green and Silverman
;; (Nonparametric Regression and Generalized Linear Models, Chapman
;; and Hall, 1994) there are two ways to compute interpolating and
;; smoothing natural cubic splines. One is efficient computationally,
;; because it uses the banded structure of the matrices, and one is
;; slower, but easier to generalize to other smoothness penalties
;; and to higher dimensions (see their Chapter 7, in particular).
;;
;; In Xlisp-Stat, we have to rely on the linear algebra routines that
;; are already coded in C, and it does not make much sense to write
;; elaborate Lisp programs using the banded structure. Thus I used
;; the slow but easily generalized approach, hoping for more appropriate
;; C support from LINPACK for banded computations in the future.
;;
;; For small examples and demos it does not really matter, of course.
;;
;; Version 1.0 -- 06-22-95 -- Basic one-dimensional natural cubic spline
;;                            smoother and interpolator.
;;
;; Version 2.0 -- 06-23-95 -- Added two-dimensional case, i.e. thin-plate
;;                            spline.
;;
;; Version 3.0 -- 06-24-95 -- General case (Green and Silverman, section
;;                            7.9), i.e. arbitrary dimension and arbitrary
;;                            spline degree.
;;
;; Version 3.1 -- 06-25-95 -- Added gamma function. Added interpolation.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun thin-plate-smooth (u z &optional (m 2) (alpha 0))
  (if (<= (* 2 m) (length (first u)))
      (error "Degree too small"))
  (let* ((npts (length z))
         (ndim (length (first u)))
         (iden (identity-matrix npts))
         (dmat (distance-matrix u u))
         (emat (+ (map-array #'(lambda (y)
                                 (smoothness y m ndim)) dmat)
                  (* alpha iden)))
         (tmat (poly-base u m))
         (nmmm (array-dimension tmat 1))
         (cmat (make-array (list (+ npts nmmm) (+ npts nmmm))
                           :initial-element 0))
         (dvec (make-list (+ npts nmmm) :initial-element 0)))
    (setf (select cmat (iseq npts) (iseq npts)) emat)
    (setf (select cmat (iseq npts) (+ npts (iseq nmmm))) tmat)
    (setf (select cmat (+ npts (iseq nmmm)) (iseq npts)) (transpose tmat))
    (setf (select dvec (iseq npts)) z)
    (solve cmat dvec)
    ))

(defun thin-plate-smoother (x u z &optional (m 2) (alpha 0))
  (let* ((coef (thin-plate-smooth u z m alpha))
         (npts (length z))
         (ndim (length (first u)))
         (iden (identity-matrix npts))
         (dmat (distance-matrix x u))
         (emat (map-array #'(lambda (y)
                                 (smoothness y m ndim)) dmat))
         (tmat (poly-base x m))
         (nmmm (array-dimension tmat 1))
         (delt (select coef (iseq npts)))
         (alph (select coef (+ npts (iseq nmmm)))))
    (+ (matmult emat delt) (matmult tmat alph))
    )
  )

(defun smoothness (x m d)
  (let ((phi (cond ((evenp d) (* (^ -1 (+ 1 m (/ d 2)))
                                 (^ 2 (- 1 (* 2 m)))
                                 (^ pi (- (/ d 2)))
                                 (/ (factorial (1- m)))
                                 (/ (factorial (- m (/ d 2))))))
                   (t (* (gamma-function (- (/ d 2) m))
                         (^ 2 (- (* 2 m)))
                         (^ pi (- (/ d 2)))
                         (/ (factorial (1- m))))))))(break)
    (if (= x 0) 0
      (cond ((evenp d) (* phi (^ x (- (* 2 m) d)) (log x)))
            (t (* phi (^ x (- (* 2 m) d)))))
      )
    )
  )

(defun poly-base (x m)
  (let ((d (length (first x))))
    (outer-product x (occupancy d m) #'(lambda (a b) (prod (^ a b))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utilities
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun map-array (func array)
  (let ((ndim (array-dimensions array))
        (ntot (array-total-size array)))
    (make-array ndim :displaced-to
                (map 'vector func 
                     (make-array ntot :displaced-to array)))
    )
  )

(defun distance-matrix (x y)
  (sqrt (map-array #'sum (^ (outer-product x y #'-) 2)))
  )

(defun factorial (n)
  (if (= n 0) 1
    (* n (factorial (1- n))))
  )

(defun occupancy (d m)
  (let ((lst (add-one nil m)))
    (loop
     (if (= d (length (first lst)))
         (return lst))
     (let ((kst nil))
       (dolist (mst lst)
         (setf kst (append kst (add-one mst m))))
       (setf lst kst)
       )
     )
    )
  )

(defun add-one (x m)
  (let ((s (if (null x) 0 (sum x))))
    (mapcar #'(lambda (y) (append (list y) x)) (iseq (- m s)))
    )
  )

(defun gamma (z)
  (cond ((< z -1.0) (/ (gamma (1+ z)) z))
	((> z  1.0) (*   (gamma (1- z)) (1- z)))
	(t (/ (gamma-reciprocal z)))
        )
  )

(defun gamma-reciprocal (x)
  (if (or (< x -1) (> x 1))
      (error "Argument out of range"))
  (if (or (= x 0) (= x -1))
      (error "Function undefined"))
               (* x
            (+ +1.0000000000000000
               (* x
	    (+ +0.5772156649015329
               (* x
	    (+ -0.6558780715202538
               (* x
	    (+ -0.0420026350340952
               (* x
	    (+ +0.1665386113822915		; 5
               (* x
	    (+ -0.0421977345555443
               (* x
	    (+ -0.0096219715278770
               (* x
	    (+ +0.0072189432466630
               (* x
	    (+ -0.0011651675918591
               (* x
	    (+ -0.0002152416741149		;10
               (* x
	    (+ +0.0001280502823882
               (* x
	    (+ -0.0000201348547807
               (* x
	    (+ -0.0000012504934821
               (* x
	    (+ +0.0000011330272320
               (* x
	    (+ -0.0000002056338417		;15
               (* x
	    (+ +0.0000000061160950
               (* x
	    (+ +0.0000000050020075
               (* x
	    (+ -0.0000000011812746
               (* x
	    (+ +0.0000000001043427
               (* x
	    (+ +0.0000000000077823		;20
               (* x
	    (+ -0.0000000000036968
               (* x
	    (+ +0.0000000000005100
               (* x
	    (+ -0.0000000000000206
               (* x
	    (+ -0.0000000000000054
               (* x
	    (+ +0.0000000000000014		;25
               (* x
	    (+ +0.0000000000000001
               ))))))))))))))))))))))))))))))))))))))))))))))))))))
   )	