;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This adds bootstrap methods to the regression-model-proto. The
;; basic bootstrap method has four options, descvribed by Hu and
;; Zidek (Biometrika, 82, 1995, 263-275), Efron (Annals of Statistics,
;; 7, 1979, 1-26, Wu (Annals of Statistics, 14, 1986, 1261-1295),
;; and Freedman (Annals of Statistics, 9, 1981, 1218-1228).
;;
;; Version 1.0 --- Jan de Leeuw -- Lost in Time
;; Version 2.0 --- 07 - 13 - 95 -- Added options, although still
;;                                 incompletely
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth regression-model-proto :bootstrap-coefficients 
  (nboot &key (method "hu-and-zidek"))
  (let* ((r (send self :raw-residuals))
         (p (send self :fit-values))
         (n (send self :num-cases))
         (m (iseq n))
         (x (send self :x-matrix))
         (z (* r (row-list x)))
         (c (send self :xtxinv))
         (u (send self :coef-estimates))
         (b (repeat nil nboot)))
    (dotimes (i nboot b)
             (setf (elt b i) 
                   (cond 
                     ((string-equal method "hu-and-zidek")
                      (let ((s (sample z n t)))
                        (+ u (matmult c (apply #'+ s)))))
                     ((string-equal method "efron")
                      (let ((s (sample r n t)))
                        (+ u (matmult c (matmult (transpose x) s)))))
                     ((string-equal method "wu")
                      (error "No Wu Yet"))
                     ((string-equal method "freedman")
                      (let* ((ns (sample m n t))
                             (xs (select x ns (iseq (array-dimension x 1))))
                             (ys (select y ns))
                             (cs (matmult (transpose xs ) xs))
                             (ds (matmult (transpose xs) ys)))
                        (matmult (inverse cs) ds)))
                     )
                   )
             )
                            
    )
  )

(defmeth regression-model-proto :prediction-error-bootstrap (nboot)
  (let* ((r (send self :raw-residuals))
         (p (send self :fit-values))
         (n (send self :num-cases))
         (x (send self :x-matrix))
         (y (send self :y))
         (s (repeat nil nboot))
         (g (matmult (send self :xtxinv) (transpose x))))
    (dotimes (i nboot)
             (let* ((yb (+ p (sample r n t)))
                    (bb (matmult g yb))
                    (hb (matmult x bb))
                    (gb (sum (^ (- y hb) 2)))
                    (rb (sum (^ (- yb hb) 2)))
                    (sb (- gb rb)))
               (setf (elt s i) sb)))
    (+ (send self :residual-sum-of-squares) (mean s))
    )
  )

(defmeth regression-model-proto :permutation-test (nperm)
  (let* ((r (send self :residual-sum-of-squares))
         (x (send self :x-matrix))
         (n (send self :num-cases))
         (y (send self :y))      
         (g (matmult (send self :xtxinv) (transpose x)))
         (s (repeat nil nperm)))
    (dotimes (i nperm)
             (let* ((ys (sample y n))
                    (bs (matmult g ys))
                    (hs (matmult x bs))
                    (rs (sum (^ (- ys hs) 2))))
               (setf (elt s i) rs)))
    (/ (length (which (< s r))) nperm)
    )
  )

(defmeth regression-model-proto :bootstrap-dispersion
  (nboot &key (method "hu-and-zidek"))
  (let* ((b (send self :bootstrap-coefficients nboot :key method))
         (m (send self :num-coefs))
         (u (make-array (list nboot m) :initial-contents b)))
    (covariance-matrix u)
    )
  )