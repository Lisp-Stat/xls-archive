;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This is the simple model for variance heterogeneity studied by 
;; Cook and Weisberg (Biometrika, 1983), and programmed as a GLIM
;; macro by Aitkin (Applied Statistics, 1987). The basic idea is
;; that $\sigma_i^2=\exp z_i'\lambda$, where $Z$ is a set of
;; predictors for the variance, which can overlap $X$, the predictors
;; for the mean.
;;
;; Version 1.0 -- 07-26-95 -- Jan de Leeuw
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hetero-regression 
  (x y z &key (verbose nil) (graphbose nil) (eps 1e-6) (itmax 20))
  (let* ((n (length y))
         (r (regression-model x y :print nil))
         (e (send r :raw-residuals))
         (g (gammareg-model z (^ e 2) :link log-link :print nil
                            :verbose nil))
         (c (send g :coef-estimates))
         (sold (exp (send g :eta)))
         snew
         (itel 1)
         (fold positive-infinity)
         fnew
         (plot (if graphbose
                   (plot-points (repeat 1 n) sold))))
    
    (loop
       (send r :weights (/ sold))
       (setf e (send r :raw-residuals))
       (send g :scale 2)
       (send g :estimate-scale nil)
       (send g :yvar (^ e 2))
       (send g :compute) 
       (setf snew (exp (send g :eta)))
       (if graphbose (progn
                      (mapcar #'(lambda (i)
                                  (send plot :add-lines 
                                        (list itel (1+ itel)) 
                                        (list (elt sold i) (elt snew i)))) 
                              (iseq n))
                      (send plot :adjust-to-data)))
       (setf fnew (sum (+ (log snew) (/ (^ e 2) snew))))
       (format t "~3d ~,10f ~,10f ~%" itel fold fnew)
       (if verbose (print (append (send r :coef-estimates)
                                  (send g :coef-estimates))))
       (if (or (= itel itmax) (> eps (abs (- fold fnew))))
               (return (progn (send g :display) (send r :display)))
           (setf itel (1+ itel) fold fnew sold snew))
     )
    )
  )
 
#|

(setf x (make-array '(20 2) :displaced-to (coerce (normal-rand 40) 'vector)))

(setf s (iseq 20))

(setf y (+ (apply #'+ (column-list x)) (* s (normal-rand 20))))

(setf z (make-array '(20 1) :initial-contents (iseq 20)))

|#
