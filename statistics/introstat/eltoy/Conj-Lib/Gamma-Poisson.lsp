;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines Beta/Binomial distribution families.

(require :el-conjugate (strcat ElToY-directory "conj" *d-sep*  "conjugate.lsp"))
(require :el-gamma (strcat ElToY-directory "Dist-Lib" *d-sep*  "Gamma.lsp"))
(require :el-Poisson (strcat ElToY-directory "Dist-Lib" *d-sep*  "Poisson.lsp"))


;;;;;; ----------------------  Gamma Prior Distribution ----------------
;; Meant to be used as prior in a gamma-Poisson family

(defproto gamma-prior-family () () (list gamma-family continuous-prior-proto)
  "Gamma prior distribution  
   Note uses beta= 1/scale instead of scale parameter.")


;;; depends on the gamma-family proto :isnew method heavily
(defmeth gamma-prior-family :isnew
  	(&rest args
	 &key (name (gensym "GAMMA-P-"))
	      (rv-names '(lambda))
	      (parameter-names '(alpha beta))
	      (default-parameters '(.5 .001))
	      (parameter-range-default '((0 300) (0 10)))
	      &allow-other-keys)
  (apply #'call-next-method
   :name name
   :rv-names rv-names
   :parameter-names parameter-names
   :default-parameters default-parameters
   :parameter-range-default parameter-range-default
   args))


(send gamma-prior-family :name :gamma-prior)
(send gamma-prior-family :isnew :name :gamma-prior)

;; note that we are now using 1/scale parameter rather than scale
;; parameter.  Need to rework those functions which are effected.

;; :quantiles  -- takes parameters as optional second argument.  First
;; argument should be probabilities expressed as fraction
(defmeth gamma-prior-family :quantiles
         (prob &key (parameters (send self :default-parameters)))
  (unless (listp prob) (setq prob (list prob)))
  (let ((shape (car parameters))
	(scale (cadr parameters)))
    (/ (gamma-quant prob shape) scale)))


;; :density  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth gamma-prior-family :density
         (quant &key (parameters (send self :default-parameters)))
   (let ((shape (car parameters))
	 (scale (cadr parameters)))
     (* (gamma-dens (* quant scale) shape)
	scale)))

;; :cdf  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth gamma-prior-family :cdf
         (quant &key (parameters (send self :default-parameters)))
   (let ((shape (car parameters))
	 (scale (cadr parameters)))
     (gamma-cdf (* quant scale) shape)))
	

;;; random variable defaults  (again parameters are optional
;;; arguements)


;; :rv-range-default
(defmeth gamma-prior-family :rv-range-default
         (&key (parameters (send self :default-parameters)))
   (let ((shape (car parameters))
	 (scale (cadr parameters)))
     (list (/ '(0 5) scale))))


(defmeth gamma-prior-family :rand (num &key
			      (parameters (send self :default-parameters)))
  (let ((shape (car parameters))
	(scale (cadr parameters)))
    (/ (gamma-rand num shape) scale)))

(defmeth gamma-prior-family :mean (&key
			      (parameters (send self :default-parameters)))
  (let ((shape (car parameters))
	(scale (cadr parameters)))
    (list (/ shape scale))))

(defmeth gamma-prior-family :variance (&key
			      (parameters (send self :default-parameters)))
  (let ((shape (car parameters))
	(scale (cadr parameters)))
    (list (/ shape scale scale))))



(push gamma-prior-family *Known-prior-families*)

;
;;;;;; Poisson Likelihood Distribution
;; Meant to be used as prior in a gamma-Poisson family

(defproto Poisson-likelihood-family () ()
  (list discrete-likelihood-proto) 
  "Poisson likelihood distribution")


;;; depends on the Poisson-family proto :isnew method heavily
(defmeth Poisson-likelihood-family :isnew
  	(&rest args
	 &key (prior)
	      (name (gensym "POISSON-L-"))
	      (rv-names '(X))
	      (nui-parameter-names '(Time))
	      (nui-default-parameters '(1))
	      (nui-parameter-range-default '((.5 25)))
	      (nui-parameter-integer? '(nil))
	      (nui-parameter-granularity '(.5))
	      (data-constraint-fun #'vacuous-data-constraint)
	      &allow-other-keys)
  (apply #'call-next-method
   :name name
   :rv-names rv-names
   :parameter-names (send prior :rv-names)
   :default-parameters (send prior :rv-default)
   :parameter-range-default (send prior :rv-range-default)
   :parameter-limits (send prior :rv-limits)
   :nui-parameter-names nui-parameter-names
   :nui-default-parameters nui-default-parameters
   :nui-parameter-limits (list (list 0 *infty*))
   :nui-parameter-range-default nui-parameter-range-default
   :nui-parameter-integer? nui-parameter-integer?
   :nui-parameter-granularity nui-parameter-granularity
   :data-constraint-fun data-constraint-fun
   args))



(send Poisson-likelihood-family :name :Poisson-likelihood)


;;; required distribution function methods

;; :quantiles  -- takes parameters as optional second argument.  First
;; argument should be probabilities expressed as fraction
(defmeth Poisson-likelihood-family :quantiles
         (prob &key (parameters (send self :default-parameters))
	                 (nui-parameters (send self :nui-default-parameters)))
   (unless (listp prob) (setq prob (list prob)))
   (let ((lambda (car parameters))
	 (T (car nui-parameters)))
     (Poisson-quant prob (* T lambda))))


;; :atoms  -- takes parameters as keyword argument.  
;; minimum mass as optional third
;; trick use normal approximation to find range of values.
(defmeth Poisson-likelihood-family :atoms
         (&key (parameters (send self :default-parameters))
	       (nui-parameters (send self :nui-default-parameters))
	       (min-mass 0.001))
   (let ((lambda (car parameters))
	 (T (car nui-parameters)))
     (if (eql 0 min-mass)
	 (iseq 0 n)
       (let ((mu (* T lambda))
	     (sigma (sqrt (* T lambda)))
	     (z (abs (normal-quant min-mass))))
	 (iseq (max 0 (floor (- (* T lambda) (* z sigma))))
	       (ceiling (+ (* T lambda) (* z sigma))))))))


;; :mass  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth Poisson-likelihood-family :mass
         (quant &key (parameters (send self :default-parameters))
		(nui-parameters (send self :nui-default-parameters)))
   (let ((lambda (car parameters))
	 (T (car nui-parameters)))
     (Poisson-pmf quant (* T lambda))))


;; :cdf  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth Poisson-likelihood-family :cdf
         (quant &key (parameters (send self :default-parameters))
		(nui-parameters (send self :nui-default-parameters)))
   (let ((lambda (car parameters))
	 (T (car nui-parameters)))
     (Poisson-cdf quant (* T lambda))))
	

;;; random variable defaults  (again parameters are optional
;;; arguements)

;; :rv-default  --- default value of rv.

(defmeth Poisson-likelihood-family :rv-default
         (&key (parameters (send self :default-parameters))
	       (nui-parameters (send self :nui-default-parameters)))
 (let ((lambda (car parameters))
       (T (car nui-parameters)))
   (list (round (* T lambda )))))


;; :rv-limits
(defmeth Poisson-likelihood-family :rv-limits
         (&key (parameters (send self :default-parameters))
	       (nui-parameters (send self :nui-default-parameters)))
   (list (list 0 *infty*)))

;; :rv-integer?
(defmeth Poisson-likelihood-family :rv-integer? (&key &allow-other-keys)
  t)

;; :rv-range-default
(defmeth Poisson-likelihood-family :rv-range-default
         (&key (parameters (send self :default-parameters))
	       (nui-parameters (send self :nui-default-parameters)))
   (let ((lambda (* (car parameters) (car nui-parameters))))
     (list 
      (force2-between (+ (* '(-3 3) (sqrt lambda)) lambda)
		     (list 0 (* 10 lambda))))))

;; :rand
(defmeth Poisson-likelihood-family :rand
         (num &key (parameters (send self :default-parameters))
		    (nui-parameters (send self :nui-default-parameters)))
   (let ((lambda (* (car parameters) (car nui-parameters))))
     (Poisson-rand num lambda)))

;; :mean

(defmeth Poisson-likelihood-family :mean
         (&key (parameters (send self :default-parameters))
		    (nui-parameters (send self :nui-default-parameters)))
   (let ((lambda (* (car parameters) (car nui-parameters))))
     (list lambda )))

;; :variance 
(defmeth Poisson-likelihood-family :variance
         (&key (parameters (send self :default-parameters))
		    (nui-parameters (send self :nui-default-parameters)))
  (let ((lambda (* (car parameters) (car nui-parameters))))
    (list lambda)))



(send Poisson-likelihood-family :isnew :name :Poisson-likelihood
      :prior gamma-prior-family)

(push Poisson-likelihood-family *Known-likelihood-families*)

;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gamma-Poisson Family
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto gamma-Poisson-family '() '() (list conjugate-family-proto))


;; :isnew method sets up defaults then inherits from conjugate-family-proto
(defmeth gamma-Poisson-family :isnew
         (&rest args
	  &key  (prior-family gamma-prior-family)
	        (likelihood-family Poisson-likelihood-family)
	  &allow-other-keys)
 (apply #'call-next-method
	:prior-family prior-family
	:likelihood-family likelihood-family 
	args))


(send gamma-Poisson-family :isnew)

;;; required link methods for conjugate-families

;;; :forward-link --- returns posterior hyperparameters from prior
;;; hyperparameters 
(defmeth gamma-Poisson-family :forward-link
  	(&rest args
	 &key  (parameters (send self :default-hyperparameters))
	       (nui-parameters (send self :nui-default-parameters))
	       (data (send self :default-data))
	 &allow-other-keys)
  (let* ((alpha (car parameters))
	 (beta (cadr parameters))
	 (T (car nui-parameters))
	 (X (+ (car data) (- T T)))	;make sure Time and X are same shape
	 (sX (if (compound-data-p X) (sum X) X))
	 (Time (if (compound-data-p X) (sum (+ T (- X X))) T)))
    (list (+ alpha sX) (+ beta Time))))


;;; :reverse-link --- returns prior hyperparameters from posterior
;;; hyperparameters 
(defmeth gamma-Poisson-family :reverse-link
  	(&rest args &key
	       parameters
	       (nui-parameters (send self :nui-parameters-default))
	       (data (send self :default-data))
	       &allow-other-keys)
  (let* ((alpha (car parameters))
	 (beta (cadr parameters))
	 (T (car nui-parameters))
	 (X (+ (car data) (- T T)))	;make sure Time and X are same shape
	 (sX (if (compound-data-p X) (sum X) X))
	 (Time (if (compound-data-p X) (sum (+ T (- X X))) T)))
    (list
     (if (< alpha sX)
		     (progn
		       (warn "gamma-Poisson:link -alpha, setting to ~S~%"
			     *epsilon*)
		       *epsilon*)
		   (- alpha sX))
     (if (< beta Time)
		     (progn
		       (warn "gamma-Poisson:link -gamma, setting to ~S~%"
			     *epsilon*)
		       *epsilon*)
		   (- beta Time)))))


(push gamma-Poisson-family *Known-conjugate-families*)

(new-provide :el-gamma-Poisson)
