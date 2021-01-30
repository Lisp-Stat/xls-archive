;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Stockpile of probability densities, cumulative distribution functions,
;; and their inverses (quantile functions). I did not do the various
;; generating functions, since their use is mainly theoretical and not
;; computational. 
;;
;; I did put in random generators, using the inversion method, 
;; wherever possible, even if it is relatively inefficient.
;;
;; Xlisp-Stat has support for beta, gamma, normal (no parameter), 
;; Cauchy (no parameter), gamma (one parameter), chi-square (one parameter), 
;; t (one parameter), Poisson (one parameter), beta (two parameters),
;; F (two parameters), binomial (two parameters), and bivnorm (one
;; parameter).
;;
;; I generalize to more parameters, if that seems desirable, changing
;; the names to new ones, different from the supported versions. And I 
;; added a slew of new ones, taken from Devroye, Johnson and Kotz, 
;; Abramowitz and Segun, and wherever I could find them.
;;
;; As a rule, I use recursive vectorization, and I check for the most
;; obvious errors.
;;
;; Version 1.0   -- 06-18-95 -- Jan de Leeuw
;;                              just collected what I had and organized it
;;                              and added some more.
;; Version 1.1   -- 06-19-95 -- added some error checks
;;                              added inverse gaussian stuff
;;                              added sum-of-uniforms
;;                              added power distribution
;; Version 1.1.a -- 06-20-95 -- added multinomial distribution
;;                              added Perks density and cdf
;;                              added hyperbolic secant
;;                              some bugs corrected
;; Version 1.1.b -- 06-21-95 -- more bugs corrected
;;                              added reading/debugging flags
;;                              added exponential power distribution
;; Version 1.1.c -- 06-23-95 -- bug fix in multinomial (Udina)
;;                              added pieces of extreme value, multinormal,
;;                              multivariate rectangular, negative
;;                              binomial
;; Version 1.1.d -- 06-28-95 -- bug fix in exponential-quant and
;;                              hyperbolic-secant-quant (Bond)
;;                              several bug-fixes in multivariate
;; Version 1.2   -- 06-29-95 -- added non-central t, chisq, and f
;;                              three parameter Weibull
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gaussian, two parameters 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gaussian-dens (x &optional (mu 0) (sigma 1))
  (if (compound-data-p x)
      (map-elements #'gaussian-dens x mu sigma)
    (progn
      (if (<= sigma 0)
          (error "Incorrect standard deviation for Gaussian"))
      (/ (normal-dens (/ (- x mu) sigma)) sigma))
    )
  )

(defun gaussian-cdf (x &optional (mu 0) (sigma 1))
  (if (compound-data-p x)
      (map-elements #'gaussian-cdf x mu sigma)
    (progn
      (if (<= sigma 0)
          (error "Incorrect standard deviation for Gaussian"))
      (normal-cdf (/ (- x mu) sigma)))
    )
  )

(defun gaussian-quant (p &optional (mu 0) (sigma 1))
  (if (compound-data-p p)
      (map-elements #'gaussian-quant p mu sigma)
    (progn
      (if (<= sigma 0)
          (error "Incorrect standard deviation for Gaussian"))
      (if (or (<= p 0) (>= p 1))
          (error "Probability not strictly between 0 and 1")) 
     (+ mu (* sigma (normal-quant p))))
    )
  )

(defun gaussian-rand (n &optional (mu 0) (sigma 1))
  (map-elements #'gaussian-quant (uniform-rand n) mu sigma)
  )

(format t "Gaussian loaded\n")                     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rectangular on [a,b]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rectangular-dens (x &optional (a 0) (b 1))
  (if (compound-data-p x)
      (map-elements #'rectangular-dens x a b)
    (progn
      (if (<= b a)
          (error "Nonsense interval for rectangular"))
      (cond ((< x a) 0)
            ((> x b) 0)
            (t (/ (- b a)))
            ))
    )
  )

(defun rectangular-cdf (x &optional (a 0) (b 1))
  (if (compound-data-p x)
      (map-elements #'rectangular-cdf x a b)
    (progn
      (if (<= b a)
          (error "Nonsense interval for rectangular"))
      (cond ((< x a) 0)
            ((> x b) 1)
            (t (/ (- x a) (- b a)))
            ))
    )
  )

(defun rectangular-quant (p &optional (a 0) (b 1))
  (if (compound-data-p p)
      (map-elements #'rectangular-quant p a b)
    (progn
      (if (<= b a)
          (error "Nonsense interval for rectangular"))
      (if (or (<= p 0) (>= p 1))
          (error "Probability not strictly between 0 and 1")) 
      (+ a (* (- b a) p)))
    )
  )


(defun rectangular-rand (n &optional (a 0) (b 1))
  (map-elements #'rectangular-quant (uniform-rand n) a b)
  )

(format t "Rectangular loaded\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exponential 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun exponential-dens (x &optional (lambda 1))
  (if (compound-data-p x)
      (map-elements #'exponential-dens x lambda)
    (progn
      (if (<= lambda 0)
          (error "Non-positive exponential parameter"))
      (cond ((< x 0) 0)
            (t (* lambda (exp (- (* lambda x)))))
            ))
    )
  )

(defun exponential-cdf (x &optional (lambda 1))
  (if (compound-data-p x)
      (map-elements #'exponential-cdf x lambda)
    (progn
      (if (<= lambda 0)
          (error "Non-positive exponential parameter"))
      (cond ((< x 0) 0)
            (t (- 1 (exp (- (* lambda x)))))
            ))
    )
  )

(defun exponential-quant (p &optional (lambda 1))
  (if (compound-data-p p)
      (map-elements #'exponential-quant p lambda)
    (progn
      (if (<= lambda 0)
          (error "Non-positive exponential parameter"))
      (if (or (<= p 0) (>= p 1))
          (error "Probability not strictly between 0 and 1")) 
      (- (/ (log (- 1 p)) lambda))
      )
    )
  )

(defun exponential-rand (n &optional (lambda 1))
  (map-elements #'exponential-quant (uniform-rand n) lambda)
  )

(format t "Exponential loaded\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rayleigh 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rayleigh-dens (x &optional (sigma 1))
  (if (compound-data-p x)
      (map-elements #'rayleigh-dens x sigma)
    (progn
      (if (<= sigma 0)
          (error "Non-positive Rayleigh parameter"))
      (let ((y (/ x sigma)))
        (cond ((< x 0) 0)
              (t (* y (exp ( - (/ (^ y 2) 2)))))
              )))
    )
  )

(defun rayleigh-cdf (x &optional (sigma 1))
  (if (compound-data-p x)
      (map-elements #'rayleigh-cdf x sigma)
    (progn
      (if (<= sigma 0)
          (error "Non-positive Rayleigh parameter"))
      (cond ((< x 0) 0)
            (t (- 1 (exp (- (/ (^ (/ x sigma) 2) 2)))))
            ))
    )
  )

(defun rayleigh-quant (p &optional (sigma 1))
  (if (compound-data-p p)
      (map-elements #'rayleigh-quant p sigma)
    (progn
      (if (<= sigma 0)
          (error "Non-positive Rayleigh parameter"))
      (if (or (<= p 0) (>= p 1))
          (error "Probability not strictly between 0 and 1")) 
      (* sigma (sqrt (- (log (- 1 p))))))
    )
  )

(defun rayleigh-rand (n &optional (sigma 1))
  (map-elements #'rayleigh-quant (uniform-rand n) sigma)
  )

(format t "Rayleigh loaded\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Triangular on [a,b] with mode at c 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun triangular-dens (x &optional (a 0) (b 1) (c .5)) 
  (if (compound-data-p x)
      (map-elements #'triangular-dens x a b c)
    (progn
      (if (not (< a c b))
          (error "Parameters for triangular not in correct order"))
      (cond ((< x a) 0)       
            ((> x b) 0)
            (t (if (<= x c) (/ (* 2 (- x a)) (* (- b a) (- c a)))
                 (/ (* 2 (- x b)) (* (- b a) (- c b)))))))
    )
  )

(defun triangular-cdf (x &optional (a 0) (b 1) (c .5))
  (if (compound-data-p x)
      (map-elements #'triangular-cdf x a b c)
    (progn
      (if (not (< a c b))
          (error "Parameters for triangular not in correct order"))
      (cond ((< x a) 0)
            ((> x b) 1)
            (t (if (<= x c) (/ (^ (- x a) 2) (* (- b a) (- c a)))
                 (/ (+ (* (- x a) (- b c)) (* (- x c) (- b x)))
                    (* (- b a) (- b c)))))))
    )
  )

(format t "Triangular loaded\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pareto 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pareto-dens (x &optional (a 1) (b 1))
  (if (compound-data-p x)
      (map-elements #'pareto-dens x a b)
    (progn 
      (cond ((< x b) 0)
        (t (/ (* a (expt b a)) (expt x (1+ a))))
        ))
    )
  )

(defun pareto-cdf (x &optional (a 1) (b 1))
  (if (compound-data-p x)
      (map-elements #'pareto-cdf x a b)
    (progn 
      (cond ((< x b) 0)
            (t (- 1 (expt (/ b x) a)))
            ))
    )
  )

(defun pareto-quant (p &optional (a 1) (b 1))
  (if (compound-data-p p)
      (map-elements #'pareto-quant p a b)
    (progn 
      (if (or (<= p 0) (>= p 1))
          (error "Probability not strictly between 0 and 1")) 
      (/ b (expt (- 1 p) (/ a))))
    )
  )

(defun pareto-rand (n &optional (a 1) (b 1))
  (map-elements #'pareto-quant (uniform-rand n) a b)
  )

(format t "Pareto loaded\n")
;; Logistic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun logistic-dens (x &optional (mu 0) (sigma 1))
  (if (compound-data-p x)
      (map-elements #'logistic-dens x mu sigma)
    (progn 
      (if (<= sigma 0)
          (error "Incorrect standard deviation for logistic"))
      (let* ((z (/ (- x mu) sigma))
             (y (exp (- z))))
        (/ y (^ (1+ y) 2) sigma)))
    )
  )

(defun logistic-cdf (x &optional (mu 0) (sigma 1))
  (if (compound-data-p x)
      (map-elements #'logistic-cdf x mu sigma)
    (progn 
      (if (<= sigma 0)
          (error "Incorrect standard deviation for logistic"))
      (/ (1+ (exp (- (/ (- x mu) sigma))))))
    )
  )

(defun logistic-quant (p &optional (mu 0) (sigma 1))
  (if (compound-data-p p)
      (map-elements #'logistic-quant p mu sigma)
    (progn 
      (if (<= sigma 0)
          (error "Incorrect standard deviation for logistic"))
      (if (or (<= p 0) (>= p 1))
          (error "Probability not strictly between 0 and 1")) 
      (+ mu (* sigma (log (/ p (- 1 p))))))
    )
  )

(defun logistic-rand (n &optional (mu 0) (sigma 1))
  (map-elements #'logistic-quant (uniform-rand n) mu sigma)
  )

(format t "Logistic loaded\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Laplace 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun laplace-dens (x &optional (mu 0) (sigma 1))
  (if (compound-data-p x)
      (map-elements #'laplace-dens x mu sigma)
    (progn 
      (if (<= sigma 0)
          (error "Incorrect standard deviation for Laplace"))
      (/ (exp (- (abs (/ (- x mu) sigma)))) (* 2 sigma)))
    )
  )

(defun laplace-cdf (x &optional (mu 0) (sigma 1))
  (if (compound-data-p x)
      (map-elements #'laplace-cdf x mu sigma)
    (progn 
      (if (<= sigma 0)
          (error "Incorrect standard deviation for Laplace"))
      (let ((y (/ (exp (- (abs (/ (- x mu) sigma)))) 2)))
        (if (<= x mu) y (- 1 y))))
    )
  )

(defun laplace-quant (p &optional (mu 0) (sigma 1))
  (if (compound-data-p p)
      (map-elements #'laplace-quant p mu sigma)
    (progn 
      (if (<= sigma 0)
          (error "Incorrect standard deviation for Laplace"))
      (if (or (<= p 0) (>= p 1))
          (error "Probability not strictly between 0 and 1")) 
      (if (<= p .5)
          (+ mu (* sigma (log (* 2 p))))
        (- mu (* sigma (log (* 2 (- 1 p)))))))
    )
  )

(defun laplace-rand (n &optional (mu 0) (sigma 1))
   (map-elements #'laplace-quant (uniform-rand n) mu sigma)
   )

(format t "Laplace loaded\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Weibull (one parameter)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun weibull-dens (x &optional (mu 0) (sigma 1) (c 1))
  (if (compound-data-p x)
      (map-elements #'weibull-dens mu sigma c)
    (progn 
      (if (<= sigma 0)
          (error "Incorrect standard deviation for Weibull"))
      (if (<= c 0)
          (error "Incorrect shape parameter for Weibull"))
      (let ((z (/ (- x mu) sigma)))
        (* c (/ sigma)
           (expt z (1- c)) 
           (exp (- (expt z c))))
        )
      )
    )
  )


(defun weibull-cdf (x &optional (mu 0) (sigma 1) (c 1))
  (if (compound-data-p x)
      (map-elements #'weibull-cdf mu sigma c)
    (progn 
      (if (<= sigma 0)
          (error "Incorrect standard deviation for Weibull"))
      (if (<= c 0)
          (error "Incorrect shape parameter for Weibull"))
      (let ((z (/ (- x mu) sigma)))
        (- 1.0 (exp (- (expt z c))))
        )
      )
    )
  )
  

(defun weibull-quant (p &optional (mu 0) (sigma 1) (c 1))
  (if (compound-data-p p)
      (map-elements #'weibull-quant mu sigma c)
    (progn 
      (if (<= sigma 0)
          (error "Incorrect standard deviation for Weibull"))
      (if (<= c 0)
          (error "Incorrect shape parameter for Weibull"))
      (if (or (<= p 0) (>= p 1))
          (error "Probability not strictly between 0 and 1")) 
      (+ mu (* sigma (expt (- (log (- 1 p))) (/ c))))
      )
    )
  )

(defun weibull-rand (n &optional (mu 0) (sigma 1) (c 1))
  (map-elements #'weibull-quant (uniform-rand n) mu sigma c)
  )

(format t "Weibull loaded\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inverse Gaussian ; compare Devroye, pages 148-150
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun inverse-gaussian-dens (x &optional (mu 1) (lambda 1))
  (if (compound-object-p x)
      (map-elements #'inverse-gaussian-dens x mu lambda)
    (progn
      (if (or (< mu 0) (< lambda 0))
          (error "Wrong parameters for inverse Gaussian"))
      (if (< x 0) 0
        (* (sqrt (/ lambda (* 2 pi (^ x 3))))
           (exp (- (/ (* lambda (^ (- x mu) 2))
                      (* x (^ mu 2)))))))
      )
    )
  )

(defun inverse-gaussian-cdf (x &optional (mu 1) (lambda 1))
  (if (compound-object-p x)
      (map-elements #'inverse-gaussian-cdf x mu lambda)
      (progn
        (if (or (< mu 0) (< lambda 0))
            (error "Wrong parameters for inverse Gaussian"))
        (if (< x 0) 0
          (+ (normal-cdf (* (sqrt (/ lambda x)) (1- (/ x mu))))
             (* (exp (/ (* 2 lambda) mu))
              (normal-cdf (- (* sqrt (/ lambda x)) (1+ (/ x mu)))))))
        )
      )
    )

(defun inverse-gaussian-quant (p &optional (mu 1) (lambda 1))
  (error "Not available. Invert numerically")
  )
      
(defun one-inverse-gaussian-rand (&optional (mu 1) (lambda 1))
  (let* ((y (^ (first (normal-rand 1)) 2))
         (h (- (+ mu (/ (* y (^ mu 2)) (* 2 lambda)))
               (* (/ mu (* 2 lambda))
                  (sqrt (+ (* 4 mu lambda y) (^ (* mu y) 2))))))
         (u (one-uniform-rand)))
    (if (<= u (/ mu (+ mu h))) h (/ (^ mu 2) h))
    )
  )
        
(defun inverse-gaussian-rand (n &optional (mu 1) (lambda 1))
 (mapcar #'(lambda (x)
             (one-inverse-gaussian-rand mu lambda))
         (make-list n))
 )

(format t "Inverse Gaussian loaded\n") 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sum of n Uniforms ; compare Devroye, pages 21-22
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun positive-part (x)
  (if (compound-data-p x)
      (map-elements #'positive-part x)
    (if (>= x 0) x 0))
  )

(defun sum-of-uniforms-dens (x &optional (n 2))
  (let ((ff 1)
        (bf 1)
        (nn (1- n))
        (ss (* n (^ (positive-part x) (1- n)))))
    (dolist (i (1+ (iseq n)))
      (setf ff (* ff i))
      (setf bf (* bf (/ (1+ (- n i)) i)))
      (if (evenp i)
          (incf ss (* n bf (^ (positive-part (- x i)) nn)))
        (decf ss (* n bf (^ (positive-part (- x i)) nn))))
      )
    (/ ss ff)
    )
  )
  
(defun sum-of-uniforms-cdf (x &optional (n 2))
  (let ((ff 1)
        (bf 1)
        (ss (^ (positive-part x) n)))
    (dolist (i (1+ (iseq n)))
      (setf ff (* ff i))
      (setf bf (* bf (/ (1+ (- n i)) i)))
      (if (evenp i)
          (incf ss (* bf (^ (positive-part (- x i)) n)))
        (decf ss (* bf (^ (positive-part (- x i)) n))))
      )
    (/ ss ff)
    )
  )

(format t "Sum of Uniforms loaded\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Power distribution ; compare Devroye, pages 24
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun power-dens (x &optional (a 0))
  (if (compound-data-p x)
      (map-elements #'power-dens x a)
    (progn
      (if (<= a -1)
          (error "Incorrect parameter power distribution"))
      (cond ((<= x 0) 0)
            ((> x 1) 0)
            (t (* (1+ a) (expt x a)))
            )
      )
    )
  )

(defun power-cdf (x &optional (a 0))
  (if (compound-data-p x)
      (map-elements #'power-cdf x a)
    (progn
      (if (<= a -1)
          (error "Incorrect parameter power distribution"))
      (cond ((<= x 0) 0)
            ((> x 1) 1)
            (t (expt x (1+ a)))
            )
      )
    )
  )

(defun power-quant (p &optional (a 0))
  (if (compound-data-p p)
      (map-elements #'power-quant p a)
    (progn
      (if (<= a -1)
          (error "Incorrect parameter power distribution"))
      (if (or (<= p 0) (>= p 1))
          (error "Probability not strictly between 0 and 1")) 
      (expt p (/ (1+ a)))
      )
    )
  )

(defun power-rand (n &optional (a 0))
  (map-elements #'power-quant (uniform-rand n) a)
  )

(format t "Power loaded\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multinomial distribution 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun multinomial-pmf (x &optional (p (/ (length n))))
  (/ (prod (poisson-pmf x p)) (poisson-pmf (sum x) 1))
  )

(defun one-multinomial-rand 
  (&optional (nn 1) (d 2) (p (make-list d :initial-element (/ d))))
  (let ((s 1)
        (m nn)
        (x (make-list d)))
    (dotimes (k d x)
      (let ((pp (elt p k)))
        (cond ((= m 0) (setf (elt x k) 0))
	      ((>= pp s);just for the case that fp arit gives that
	       (decf m (setf (elt x k) m))
	       (decf s pp))
              (t (decf m (setf (elt x k) 
                               (first (binomial-rand  1 m (/ pp s)))))
                 (decf s pp)))) 
      )
    )
  )

(defun multinomial-rand 
  (n &optional (nn 1) (d 2) (p (make-list d :initial-element (/ d))))
 (mapcar #'(lambda (x)
             (one-multinomial-rand nn d p))
         (make-list n))
 )   

(format t "Multinomial loaded\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Perks distribution (Devroye, p. 287, p. 472)
;; For teaching purposes, the a parameter could be put on a slider 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
(defun perks-dens (x &optional (mu 0) (sigma 1) (a 2))
  (if (compound-data-p x)
      (map-elements #'perks-dens x mu sigma a)
    (progn
      (if (<= a -2)
          (error "Incorrect shape parameter Perks distribution"))
      (if (<= sigma 0)
          (error "Incorrect sigma parameter Perks distribution"))
      (let* ((z (/ (- x mu) sigma))
             (b (/ a 2))
             (k (cond ((= a 2) 0)
                      ((< a 2) (sqrt (- 1 (^ b 2))))
                      (t (sqrt (- (^ b 2) 1)))))
             (c (cond ((= a 2) 1)
                      ((< a 2) (/ (- (/ pi 2) (atan (/ b k))) k)) 
                      (t (/ (log (/ (abs (+ k b)) (abs (- k b)))) 2 k)))))
        (/ (* c sigma (+ (exp z) (exp (- z)) a))))
      )
    )
  )

(defun perks-cdf (x &optional (mu 0) (sigma 1) (a 2))
  (if (compound-data-p x)
      (map-elements #'perks-cdf x mu sigma a)
    (progn
      (if (<= a -2)
          (error "Incorrect shape parameter Perks distribution"))
      (if (<= sigma 0)
          (error "Incorrect sigma parameter Perks distribution"))
      (let* ((z (/ (- x mu) sigma))
             (b (/ a 2))
             (k (cond ((= a 2) 0)
                      ((< a 2) (sqrt (- 1 (^ b 2))))
                      (t (sqrt (- (^ b 2) 1)))))
             (c (cond ((= a 2) 1)
                      ((< a 2) (/ (- (/ pi 2) (atan (/ b k))) k)) 
                      (t (/ (log (/ (abs (+ k b)) (abs (- k b)))) 2 k)))))
         (cond ((= a 2) (logistic-cdf x mu sigma)) 
               ((< a 2) (/ (- (atan (/ (+ b (exp x)) k))
                              (atan (/ b k))) (* c k)))
               (t (/ (- (log (/ (abs (+ k b)) 
                                (abs (- k b))))
                        (log (/ (abs (+ k (+ b (exp x))))
                                (abs (- k (+ b (exp x))))))) (* 2 c k)))
               )
         )                          
      )
    )
  )

(format t "Perks loaded\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hyperbolic secant distribution (Devroye, p. 471-472) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hyperbolic-secant-dens (x &optional (mu 0) (sigma 0))
   (if (compound-data-p x)
      (map-elements #'hyperbolic-secant-dens x mu sigma)
     (progn
       (if (<= sigma 0)
           (error "Incorrect sigma parameter hyperbolic secant distribution"))
       (let ((z (/ (- x mu) sigma)))
         (/ 2 (* sigma pi (+ (exp z) (exp (- z))))))
      )
     )
   ) 

(defun hyperbolic-secant-cdf (x &optional (mu 0) (sigma 0))
   (if (compound-data-p x)
      (map-elements #'hyperbolic-secant-cdf x mu sigma)
     (progn
       (if (<= sigma 0)
           (error "Incorrect sigma parameter hyperbolic secant distribution"))
       (let ((z (/ (- x mu) sigma)))
         (/ (* 2 (atan (exp z))) pi))
       )
     )
   ) 

(defun hyperbolic-secant-quant (p &optional (mu 0) (sigma 0))
   (if (compound-data-p p)
      (map-elements #'hyperbolic-secant-quant p mu sigma)
     (progn
       (if (<= sigma 0)
           (error "Incorrect sigma parameter hyperbolic secant distribution"))
       (+ mu (* sigma (log (tan (* (/ pi 2) p)))))
       )
     )
   )

(defun hyperbolic-secant-rand (n &optional (mu 0) (sigma 0))
  (map-elements #'hyperbolic-secant-quant (uniform-rand n) mu sigma)
  )

(format t "Hyperbolic Secant loaded\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exponential Power distribution (Devroye, p. 287) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gamma (x)
  (exp (log-gamma x)))

(defun exponential-power-dens (x &optional (mu 0) (sigma 1) (a 2))
  (if (compound-data-p x)
      (map-elements #'exponential-power-dens x mu sigma a)
    (progn 
      (if (< sigma 0)
           (error "Incorrect sigma parameter exponential power distribution"))
      (if (< a 1)
           (error "Incorrect shape parameter exponential power distribution"))
      (let ((z (/ (- x mu) sigma)))
        (/ (exp (- (expt (abs z) a))) (* 2 sigma (gamma (1+ (/ a))))))
      )
    )
  )

(defun exponential-power-cdf (x &optional (mu 0) (sigma 1) (a 2))
  (if (compound-data-p x)
      (map-elements #'exponential-power-cdf x mu sigma a)
    (progn 
      (if (< sigma 0)
           (error "Incorrect sigma parameter exponential power distribution"))
      (if (< a 1)
           (error "Incorrect shape parameter exponential power distribution"))
      (let ((z (/ (- x mu) sigma)))
        (if (>= z 0)
            (/ (+ 1 (gamma-cdf z (/ a))) 2)
          (/ (- 1 (gamma-cdf (- z) (/ a))) 2)))
      )
    )
  )

(defun one-exponential-power-rand (&optional (mu 0) (sigma 1) (a 2))
  (+ mu (* sigma (expt (first (gamma-rand (1+ (/ a)))) (/ a))
           (- (* 2 (random 1.0)) 1)))
  )

(format t "Exponential Power loaded\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extreme value distribution (Devroye, p. 287) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extreme-value-dens (x &optional (mu 0) (sigma 1) (k 1))
  (if (compound-data-p x)
      (map-elements #'extreme-value-dens x mu k)
    (progn 
      (if (< sigma 0)
           (error "Incorrect sigma parameter extreme-value distribution"))
      (if (or (not (integerp k)) (< k 1))
           (error "Incorrect shape parameter extreme-value distribution"))
      (* (exp (- (* k (log k)) (log-gamma k)))
         (exp (- (* k (+ x (exp (- x)))))))
      )
    )
  )

(format t "Extreme Value loaded\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generalized Inverse Gaussian  distribution (Devroye, p. 287) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(format t "Generalized Inverse Gaussian loaded\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Negative binomial distribution  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun negative-binomial-pmf (x &optional (n 1) (p .5))
  (if (compound-data-p x)
      (map-elements #'negative-binomial-pmf x n p)
    (progn 
      (if (or (< p 0) (> p 1))
          (error "Incorrect p parameter negative binomial distribution"))
      (if (or (not (integerp n)) (< n 0))
          (error "Incorrect n parameter negative binomial distribution"))
      (* (/ n (+ n x)) (binomial-pdf n (+ n x) p))
      )
    )
  )

(format t "Negative Binomial loaded\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multivariate normal distribution  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun multinormal-dens 
  (x &optional (mu (repeat 0 length x))
     (sigma (identity-matrix (length x))))
  (let ((ndm (length x))
        (inv (inverse sigma))
        (det (determinant sigma)))
    (* (expt (* 2 pi det) (/ ndm 2))
       (exp (* -.5 (matmult (- x mu) (matmult inv (- x mu))))))
    )
  )

(defun independent-normal-dens (x mu sigma)
  (x &optional (mu (repeat 0 length x))
                (sigma (repeat 1ength x)))
  (let* ((zz (/ (- x mu) (sqrt sigma)))
         (pp (/ (normal-dens zz) (sqrt sigma))))
    (prod pp)
    )
  )

(defun multinormal-rand 
  (&optional (ndim 1)(mu (repeat 0 ndim)) (sigma (identity-matrix ndim)))
  (let ((s (chol-decomp sigma)))
    (+ mu (matmult s (normal-rand ndim))))
  )

(defun independent-normal-rand (mu sigma)
  (&optional (ndim 1)(mu (repeat 0 ndim)) (sigma (repeat 1 ndim)))
  (+ mu (* (sqrt sigma) (normal-rand ndim)))
  )

(format t "Multivariate Normal loaded\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multivariate Rectangular distribution  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun multiuniform-dens (d)
  (&optional (ndim 1) (a (repeat 0 ndim)) (b (repeat 1 ndim)))
  (/ (prod (- b a)))
)

(defun multiuniform-rand 
  (&optional (ndim 1) (a (repeat 0 ndim)) (b (repeat 1 ndim)))
  (+ a (* (- b a) (mapcar #'random (repeat 1.0 ndim))))
)

(format t "Multivariate Rectangular loaded\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Noncentral t (series, Johnson and Kotz, CUD, II, 204-205)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun noncentral-t-between-plus-and-minus-x 
  (x &optional (df 1) (delta 0) (eps 1e-6))
  (let* ((y (/ (* x x) (+ df (* x x))))
         (j 1)
         (f 1)
         (m 1)
         (c (/ df 2))
         (s (beta-cdf y .5 c))
         (r (/ (* delta delta) 2))
         (e 0))
    (loop
     (setf f (* f j))
     (setf m (* m r))
     (setf e (/ (* m (beta-cdf y (+ j .5) c)) f))
     (if (< e eps) (return (/ s (exp r))))
     (incf s e)
     (incf j)
     )
    )
  )

(defun noncentral-t-between-zero-and-x
  (x &optional (df 1) (delta 0) (eps 1e-6))
  (let* ((y (/ (* x x) (+ df (* x x))))
         (j 1)
         (f-o (sqrt pi))
         (f-e 1)
         (m 1)
         (c (/ df 2))
         (s (beta-cdf y .5 c))
         (r (/ (* delta delta) 2))
         (g (sqrt r))
         (e 0)
         (f 0))
    (loop
     (cond ((evenp j) (setf f-e (* f-e (/ j 2))) (setf f f-e))
            (t (setf f-o (* f-o (/ j 2))) (setf f f-o)))
     (setf m (* m g))
     (setf e (/ (* m (beta-cdf y (/ (1+ j) 2) c)) f))
     (if (< e eps) (return (/ s (* 2 (exp r)))))
     (incf s e)
     (incf j)
     )
    )
  )

(defun noncentral-t-less-than-zero 
  (&optional (delta 0))(noncentral-t-cdf -1 3 0)
  (normal-cdf (- delta))
  )

(defun noncentral-t-cdf
    (x &optional (df 1) (delta 0) (eps 1e-6))
    (if (< x 0)
        (let ((all (noncentral-t-less-than-zero delta))
              (bet (noncentral-t-between-plus-and-minus-x x df delta eps))
              (ovr (noncentral-t-between-zero-and-x x df delta eps)))
          (print (list all bet ovr))
          (- all (- bet ovr)))
      (+ (noncentral-t-less-than-zero delta)
         (noncentral-t-between-zero-and-x x df delta eps))
      )
    )


(defun noncentral-t-dens 
  (x &optional (df 1) (delta 0) (eps 1e-6))
  (let* ((y (/ df (+ df (* x x))))
         (j 1)
         (f 1)
         (m 1)
         (c (/ df 2))
         (s (beta-dens y c .5))
         (r (/ (* delta delta) 2))
         (e 0))
    (loop
     (setf f (* f j))
     (setf m (* m r))
     (setf e (/ (* m (beta-dens y c (+ j .5))) f))
     (if (< e eps) (return (- 1.0 (/ s (* 2 (exp r))))))
     (incf s e)
     (incf j)
     )
    )
  )

(format t "Noncentral t loaded\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Noncentral beta (code by Russ Lenth based on App Stat Alg AS 226)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun *ncbeta* (x a b lambda)
  (cond
    ((<= x 0) 0)
    ((>= x 1) 1)
    (t (do*
        ((n 0 (+ n 1))
         (c (/ lambda 2) c)
         (ix (beta-cdf x a b) (- ix gam))
         (gam (exp (+ (- (log-gamma (+ a b))
                         (log-gamma (1+ a)) (log-gamma b))
                      (* a (log x)) 
                      (* b (log (- 1 x)))))
              (/ (* x gam (+ a b n -1)) (+ a n)))
         (q (exp (- c)) (/ (* q c) n))
         (sumq (- 1 q) (- sumq q))
         (p (* q ix) (+ p (* q ix))))
        ((< (* sumq (- ix gam)) 1e-8) p)))))


;;; vectorized version
(defun ncbeta (x a b lambda)
"Noncentral beta cdf - Args: x a b lambda"
  (map-elements #'*ncbeta* x a b lambda))

(format t "Noncentral beta loaded\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Noncentral F (code by Russ Lenth based on App Stat Alg AS 226)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun noncentral-f-cdf 
  (x &optional (df1 1) (df2 1) (lambda 0) (eps 1e-6))
  (let ((j 1)
        (f 1)
        (m 1)
        (s (f-cdf x df1 df2))
        (r (/ lambda 2))
        (e 0))
    (loop
     (setf f (* f j))
     (setf m (* m r))
     (setf e (/ (* m (f-cdf x (+ df1 j j) df2)) f))
     (if (< e eps) (return (/ s (exp r))))
     (incf s e)
     (incf j)
     )
    )
  )

(defun noncentral-f-dens 
  (x &optional (df1 1) (df2 1) (lambda 0) (eps 1e-6))
  (let ((j 1)
        (f 1)
        (m 1)
        (s (f-dens x df1 df2))
        (r (/ lambda 2))
        (e 0))
    (loop
     (setf f (* f j))
     (setf m (* m r))
     (setf e (/ (* m (f-dens x (+ df1 j j) df2)) f))
     (if (< e eps) (return (/ s (exp r))))
     (incf s e)
     (incf j)
     )
    )
  )

(format t "Noncentral F loaded\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Noncentral chi-square (Johnson and Kotz, CUD, II, 132
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun noncentral-chisq-cdf   
  (x &optional (df 1) (lambda 0) (eps 1e-6))
  (let ((j 1)
        (f 1)
        (m 1)
        (s (chisq-cdf x df))
        (r (/ lambda 2))
        (e 0))
    (loop
     (setf f (* f j))
     (setf m (* m r))
     (setf e (/ (* m (chisq-cdf x (+ df j j))) f))
     (if (< e eps) (return (/ s (exp r))))
     (incf s e)
     (incf j)
     )
    )
  )

(defun noncentral-chisq-dens   
  (x &optional (df 1) (lambda 0) (eps 1e-6))
  (let ((j 1)
        (f 1)
        (m 1)
        (s (chisq-dens x df))
        (r (/ lambda 2))
        (e 0))
    (loop
     (setf f (* f j))
     (setf m (* m r))
     (setf e (/ (* m (chisq-dens x (+ df j j))) f))
     (if (< e eps) (return (/ s (exp r))))
     (incf s e)
     (incf j)
     )
    )
  )
 
(format t "Noncentral chi-square loaded\n")
      
