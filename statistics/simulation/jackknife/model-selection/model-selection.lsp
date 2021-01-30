;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The two key defuns estimate (expected) prediction error
;; specification error and overall error for multinomial
;; and covariance matrix models.
;;
;; The theory is in "Models as Instruments", which can be
;; read or copied as http://www.stat.ucla.edu/papers/papers/168.ps
;;
;; Jan de Leeuw, 03-07-95, deleeuw@stat.ucla.edu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun multinomial-model-selection (f p d n)
"Args:
Applies the function f to the vector of
proportions p, and measures the distance
d between p and f(p). Proportions are based
on n independent observations. Then does the same
with the vectors p_i obtained by eliminating
one observation, and uses the jackknife to 
estimate various error-measures."
(let* (
      (fp (funcall f p))
      (dp (funcall d p fp))
      (m (length p))
      (jn 0)
      (kn 0)
      (ln 0)
      )
(dotimes (j m)
(let* (
      (pj (+ p (/ (- p (unit j m)) (1- n))))
      (fj (funcall f pj))
      (djf (funcall d pj fp))
      (dfj (funcall d p fj))
      (djj (funcall d pj fj))
      )
(incf jn (* (elt p j) djf))
(incf kn (* (elt p j) dfj))
(incf ln (* (elt p j) djj))
))
(setf jn (* 2 (^ (1- n) 2) (- jn dp)))
(setf kn (* 2 (^ (1- n) 2) (- kn dp)))
(setf ln (* 2 (^ (1- n) 2) (- ln dp)))
(values 
(/ (- (* 2 n dp) ln) 2)
(/ (- (* 2 n dp) (- ln kn)) 2)
(/ (+ (* 2 n dp) (- (+ jn kn) ln)) 2))
))

(defun cross-product-model-selection (f x d)
  (let* (
        (cv (cross-product-matrix x))
        (n (first (array-dimensions x)))
        (fc (funcall f cv))
        (dc (funcall d cv fc))
        (jn 0)
        (kn 0)
        (ln 0)
        )
    (dotimes (i n)
      (let* (
             (ci (cross-delete cv (elt x i) n))
             (fi (funcall f ci))
             (dif (funcall d ci fc))
             (dfi (funcall d cv fi))
             (dii (funcall d ci fi))
             )
        (incf jn dif)
        (incf kn dfi)
        (incf ln dii)
        ))
    (setf jn (* 2 (^ (1- n) 2) (- (/ jn n) dc)))
    (setf kn (* 2 (^ (1- n) 2) (- (/ kn n) dc)))
    (setf ln (* 2 (^ (1- n) 2) (- (/ ln n) dc)))
    (values 
     (/ (- (* 2 n dc) ln) 2)
     (/ (- (* 2 n dc) (- ln kn)) 2)
     (/ (+ (* 2 n dc) (- (+ jn kn) ln)) 2))
    ))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utilities
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unit (k n)
"Args: k
Returns k-th unit vector of length n"
(if-else (= k (iseq n)) (repeat 1 n) (repeat 0 n))
)

(defun cross-product-matrix (x)
  (/ (matmult (transpose x) x) 
     (first (array-dimensions x)))
  )

(defun cross-delete (c x n)
  (/ (- (* n c) (outer-product x x)) (1- n))
  )

(defun trace (c)
  (sum (diagonal c))
  xsy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Multinomial distances
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hellinger-distance (p q)
(* 4 (sum (^ (- (sqrt p) (sqrt q)) 2)))
)

(defun multinomial-likelihood-distance (p q)
  (* 2 (sum (* p (- (log p) (log q)))))
)

(defun kullback-distance (p q)
  (likelihood-distance q p)
)

(defun chi-squared-distance (p q)
  (sum (/ (^ (- p q) 2) q))
)

(defun neyman-distance (p q)
  (chi-squared-distance q p)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Cross-product distances
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun multinormal-likelihood-distance (c d)
  (+ (log (/ (determinant c) (determinant d)))
     (- (trace (matmult (inverse c) d)) 
        (array-dimension c 1)))
  )

(defun generalized-least-squares-distance (c d)
  (let* ((e (matmult (inverse c) d))
         (i (identity-matrix
             (array-dimension c 1)))
         (h (- i e)))
    (trace (matmult h h))
    )
  ) 

(defun modified-generalized-least-squares-distance (c d)
  (generalized-least-squares-distance d c)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Multinomial maps
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun identity (p)
p)

(defun independent (p)
(let* (
      (p11 (elt p 0))
      (p12 (elt p 1))
      (p21 (elt p 2))
      (p22 (elt p 3))
      (p1* (+ p11 p12))
      (p2* (+ p21 p22))
      (p*1 (+ p11 p21))
      (p*2 (+ p12 p22))
      )
(list (* p1* p*1) (* p1* p*2) (* p2* p*1) (* p2* p*2))
))

(defun equalprob (p)
  (repeat (/ (length p)) (length p))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  
;; Cross-product maps                 
;;                                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun diagonal (c)
(* c (identity-matrix (first (array-dimensions c))))
)



