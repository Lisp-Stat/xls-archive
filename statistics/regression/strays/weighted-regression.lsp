;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; For multivariate regression, repeated measurements, hierarchical
;; models, multinomial GLMS, Zeeger-Liang longitudinal GLMS, and
;; so on, we need a weighted regression proto with non-diagonal
;; (often block-diagonal) weights.
;; 
;; I started working on such a proto today until it dawned on me
;; that regression-model-proto could be used with only very minor
;; modifications. I only needed to change the make-sweep-matrix
;; defun, which now does its computations more generally 
;; (depending on whether the weights are a vector, a matrix,
;; or a block-diagonal matrix (which is stored as a list
;; of blocks).
;; 
;; A tiny modification in the compute method was needed as well,
;; because excluding variables is setting weights equal to zero.
;; 
;; And finally the residuals method computes residuals by
;; premultiplying raw residuals by the sqrt of the weights.
;; We generalize this by using chol-decomp.
;;
;; Also observe that GLIM inherits from the regression model
;; proto, so it can also do most of its things more generally
;; (I may have to iron out some details).
;;
;; Again, this shows the powers of OOP (and of sweeping, of
;; course).
;;
;; Jan de Leeuw, 02-27-95
;;
;; Overwrote the diagnostics (leverages, Cooks distances, studentized
;; residuals) inserting the necessary disclaimers. They need to be
;; adapted. Removed a residual normalization bug. Also made all this
;; into a weighted-regression-model-proto, so that we do not override
;; what is there. I think this is nicer and cleaner.
;;
;; Jan de Leeuw, 03-14-95
;;
;; Version 1.0 ** March 25 1995 **
;;             Added weighted-regression-model function
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto weighted-regression-model-proto
  () () regression-model-proto)

(defmeth weighted-regression-model-proto :compute ()
"Message args: ()
Recomputes the estimates. For internal use by other messages"
  (let* ((included (if-else (send self :included) 1 0))
         (x (send self :x))
         (y (send self :y))
         (intercept (send self :intercept))
         (weights (send self :weights))
         (w (exclude-vars weights included))
         (m (make-sweep-matrix x y w))
         (n (array-dimension x 1))
         (p (- (array-dimension m 0) 1))
         (tss (aref m p p))
         (tol (* .0001 (mapcar #'standard-deviation (column-list x))))
         (sweep-result
          (if intercept
              (sweep-operator m (iseq 1 n) tol)
              (sweep-operator m (iseq 0 n) (cons 0.0 tol)))))
    (setf (slot-value 'sweep-matrix) (first sweep-result))
    (setf (slot-value 'total-sum-of-squares) tss)
    (setf (slot-value 'residual-sum-of-squares) 
          (aref (first sweep-result) p p))
    (setf (slot-value 'basis)
          (let ((b (remove 0 (second sweep-result))))
            (if b 
                (- (reverse b) 1)
                (error "no columns could be swept"))))))

(defmeth weighted-regression-model-proto :residuals ()
(let ((raw-residuals (send self :raw-residuals))
      (weights (send self :weights)))
(cond 
  ((vectorp weights) (* (sqrt weights) raw-residuals))
  ((matrixp weights) (* (transpose (first (chol-decomp weights)))
                       raw-residuals))
  ((bmatrixp weights) (bnormalize weights raw-residuals))
  ((null weights) raw-residuals)
  (t (error "Wrong format for weights")))
))

(defmeth weighted-regression-model-proto :leverages ()
  (let ((weights (send self :weights)))
    (if (or (matrixp weights) (bmatrixp weights))
        (error "No Leverages for Non-Diagonal Weights")
        (call-next-method))))

(defmeth weighted-regression-model-proto :studentized-residuals ()
  (let ((weights (send self :weights)))
    (if (or (matrixp weights) (bmatrixp weights))
        (error "No Studentized Residuals for Non-Diagonal Weights")
        (call-next-method))))

(defmeth weighted-regression-model-proto :externally-studentized-residuals ()
  (let ((weights (send self :weights)))
    (if (or (matrixp weights) (bmatrixp weights))
        (error "No Externally Studentized Residuals for Non-Diagonal Weights")
        (call-next-method))))

(defmeth weighted-regression-model-proto :cooks-distances ()
  (let ((weights (send self :weights)))
    (if (or (matrixp weights) (bmatrixp weights))
        (error "No Cooks Distances for Non-Diagonal Weights")
        (call-next-method))))

(defun make-sweep-matrix (x y &optional w)
 (let* (
        (m (second (array-dimensions x)))
        (n (length y))
        (c (make-array (list (+ 2 m) (+ 2 m))))
        )
(setf (aref c 0 0) (if w (sum w) n))
(setf (select c 0 (1+ (iseq m)))
      (cond
        ((vectorp w) (vec-to-arr (matmult w x)))
        ((bmatrixp w) (vec-to-arr (apply #'+ (row-list (bmatmult w x)))))
        ((matrixp w) (vec-to-arr (apply #'+ (row-list (matmult w x)))))
        ((null w) (vec-to-arr (apply #'+ (row-list x))))
        (t (error "Wrong format for weights"))))
(setf (select c (1+ (iseq m)) 0)
      (transpose (select c 0 (1+ (iseq m)))))
(setf (aref c 0 (+ 1 m))
      (cond 
        ((vectorp w) (sum (* w y)))
        ((bmatrixp w) (sum (bmatmult w y)))
        ((matrixp w) (sum (matmult w y)))
        ((null w) (sum y))
        (t (error "Wrong format for weights"))))
(setf (aref c (+ 1 m) 0) (aref c 0 (+ 1 m)))
(setf (select c (1+ (iseq m)) (1+ (iseq m)))
      (cond
        ((vectorp w) (matmult (transpose x) 
                              (* x (outer-product w (repeat 1 m)))))
        ((bmatrixp w) (matmult (transpose x) (bmatmult w x)))
        ((matrixp w) (matmult (transpose x) (matmult w x)))
        ((null w) (matmult (transpose x) x))
        (t (error "Wrong format for weights"))))
(setf (select c (+ 1 m) (1+ (iseq m)))
      (cond
        ((vectorp w) (vec-to-arr (matmult (* w y) x)))
        ((bmatrixp w) (vec-to-arr (matmult (bmatmult w y) x)))
        ((matrixp w) (vec-to-arr (matmult (matmult w y) x)))
        ((null w) (vec-to-arr (matmult y x)))
        (t (error "Wrong format for weights"))))
(setf (select c (1+ (iseq m)) (+ 1 m)) 
      (transpose (select c  (+ 1 m) (1+ (iseq m)))))
(setf (aref c (+ 1 m) (+ 1 m))
      (cond
        ((vectorp w) (sum (* w y y)))
        ((bmatrixp w) (sum (* y (bmatmult w y))))
        ((matrixp w) (sum (* y (matmult w y))))
        ((null w) (sum (* y y)))
        (t (error "Wrong format for weights"))))
(first (sweep-operator c '(0)))
))

(defun weighted-regression-model (x y &key 
                           (intercept T) 
                           (print T) 
                           weights
                           (included (repeat t (length y)))
                           predictor-names
                           response-name
                           case-labels)
  (let ((x (cond 
             ((matrixp x) x)
             ((vectorp x) (list x))
             ((and (consp x) (numberp (car x))) (list x))
             (t x)))
        (y (coerce y 'vector))
        (m (send weighted-regression-model-proto :new)))
    (send m :x (if (matrixp x) x (apply #'bind-columns x)))
    (send m :y y)
    (send m :intercept intercept)
    (send m :weights weights)
    (send m :included included)
    (send m :predictor-names predictor-names)
    (send m :response-name response-name)
    (send m :case-labels case-labels)
    (if print (send m :display))
    m))

(defun vec-to-arr (x)
  (make-array (list 1 (length x))
      :displaced-to x))      

(defun bmatmult (w x)
(let* (
      (y (make-array (array-dimensions x)))
      (m (second (array-dimensions x)))
      (l (if m (iseq m) nil))
      (k 0)
      )
(dolist (v w)
(let (
     (i (first (array-dimensions v)))
     )
(setf (select y (+ k (iseq i)) l)
      (matmult v (select x (+ k (iseq i)) l)))
(incf k i)))
y))

(defun bmatrixp (w) 
"Args: W
Returns t if w is a list of square matrices"
(if (listp w)
    (if (fand (mapcar #'matrixp w))
        (fand (apply #'= (mapcar #'array-dimensions w)))))
)

(defun exclude-vars (weights included)
(cond 
  ((vectorp weights) (* included weights))
  ((matrixp weights) (* weights (outer-product included included)))
  ((bmatrixp weights) (bzero-out weights included))
  ((null weights) (coerce included 'vector))
  (t (error "Wrong format for weights")))
)

(defun bzero-out (weights included)
(let (
      (k 0)
      )
(dotimes (i (length weights))
(let* ( 
       (v (elt weights i))
       (m (first (array-dimensions v)))
       (l (+ k (iseq m)))
       (s (select included l))
       )
(setf (elt weights i) (* v (outer-product s s)))
(incf k m)))
weights))

(defun bnormalize (weights raw-residuals)
(let (
      (k 0)
      (r (copy-vector raw-residuals))
      )
(dotimes (i (length weights) r)
(let* ( 
       (v (elt weights i))
       (m (first (array-dimensions v)))
       (l (+ k (iseq m)))
       )
(setf (select r l) (matmult (transpose (first (chol-decomp v)))
                            (select r l)))
(incf k m)))
))

(defun fand (lst)
  (if (single lst) (car lst)
      (and (car lst) (fand (cdr lst))))
)

(defun single (lst)
  (and (consp lst) (not (cdr lst))))

(provide "weighted-regression")


