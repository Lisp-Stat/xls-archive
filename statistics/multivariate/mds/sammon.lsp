;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This is the MDS technique proposed by Sammon in 1962. It is just like
;; ordinary metric MDS, except that it emphasizes two dimensional
;; representations, and normalizes the loss function in a particular
;; way.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sammon-mapping 
  (dist &key (ndim 2) (eps1 1e-6) (eps2 1e-4) (itmax 100) (verbose t))
  (let* ((x (torgerson-mapping dist ndim))
         (d (distance-matrix x))
         (w (/ (map-array #'make-zero-one dist)))
         (v (norm-weights w))
         (s (sum dist))
         (f (/ (sum (* w (^ (- dist d) 2))) s))
         (i 0))
    (loop
     (let* ((b (transform-matrix w dist d))
            (y (matmult v (matmult b x)))
            (e (distance-matrix y))
            (g (/ (sum (* w (^ (- dist e) 2))) s)))
       (if (or (and (> eps1 (abs (- f g))) (> eps2 (max (abs (- x y)))))
               (= i itmax)) (return y)
         (progn
           (if verbose (format t "iteration ~4d loss ~20,10f~%" i f))
           (setf x y)
           (setf d e)
           (setf f g)
           (incf i)))
       )
     )
    )
  )

(defun torgerson-mapping (dist &optional (ndim 2))
  (let* ((nn (array-dimension dist 0))
         (ii (center-operator nn))
         (cc (* -0.5 (matmult ii (matmult (^ dist 2) ii))))
         (ee (eigen cc))
         (ea (diagonal (sqrt (select (first ee) (iseq ndim)))))
         (ev (apply #'bind-columns
                    (select (second ee) (iseq ndim)))))
    (matmult ev ea)
    )
  )

;;;;;;;;;;;;; Main MDS utilities

(defun distance-matrix (x)
  (let ((y (row-list x)))
    (sqrt (map-array #'sum (^ (outer-product y y #'-) 2)))
    )
  )

(defun transform-matrix (w d e)
  (let* ((a (* w (/ d (map-array #'make-zero-one e))))
         (h (mapcar #'sum (row-list a))))
    (- (diagonal h) a)
    )
  )

(defun norm-weights (w)
  (let* ((h (mapcar #'sum (row-list w)))
         (u (- (diagonal h) w))
         (n (array-dimension w 1)))
    (- (inverse (+ u (/ n))) (/ n))
    )
  )

;;;;;;;;;;;;; general purpose utilities

(defun map-array (func array)
  (let ((ndim (array-dimensions array))
        (ntot (array-total-size array)))
    (make-array ndim :displaced-to
                (map 'vector func 
                     (make-array ntot :displaced-to array)))
    )
  )

(defun make-zero-one (x)
  (if (= x 0) 1 x)
  )

(defun center-operator (n)
  (- (identity-matrix n) (/ n))
  )



