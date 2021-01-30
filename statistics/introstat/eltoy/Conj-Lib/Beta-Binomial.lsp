;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines Beta/Binomial distribution families.

(require :el-conjugate (strcat ElToY-directory "conj" *d-sep*  "conjugate.lsp"))
(require :el-beta (strcat ElToY-directory "Dist-Lib" *d-sep*  "Beta.lsp"))
(require :el-binomial (strcat ElToY-directory "Dist-Lib" *d-sep*  "Binomial.lsp"))


;;;;;; ----------------------  Beta Prior Distribution ----------------
;; Meant to be used as prior in a beta-binomial family

(defproto beta-prior-family () () (list beta-family continuous-prior-proto)
  "Beta prior distribution")


;;; depends on the beta-family proto :isnew method heavily
(defmeth beta-prior-family :isnew
  	(&rest args
	 &key (name (gensym "BETA-P-"))
	      (rv-names '(theta))
	      (parameter-names '(alpha beta))
	      (default-parameters '(.5 .5))
	      &allow-other-keys)
  (apply #'call-next-method
   :name name
   :rv-names rv-names
   :parameter-names parameter-names
   :default-parameters default-parameters
   args))


(send beta-prior-family :name :beta-prior)
(send beta-prior-family :isnew :name :beta-prior)

(push beta-prior-family *Known-prior-families*)

;
;;;;;; Binomial Likelihood Distribution
;; Meant to be used as prior in a beta-binomial family

(defproto binomial-likelihood-family () ()
  (list discrete-likelihood-proto) 
  "Binomial likelihood distribution")


;;; depends on the binomial-family proto :isnew method heavily
(defmeth binomial-likelihood-family :isnew
  	(&rest args
	 &key (prior)
	      (name (gensym "BINOMIAL-L-"))
	      (rv-names '(X))
	      (nui-parameter-names '(N))
	      (nui-default-parameters '(10))
	      (nui-parameter-range-default '((0 25)))
	      (nui-parameter-integer? '(t))
	      (nui-parameter-granularity '(1))
	      (data-constraint-fun #'max-data-constraint)
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
   :data-constraint-fun #'max-data-constraint
   args))



(send binomial-likelihood-family :name :binomial-likelihood)


;;; required distribution function methods

;; :quantiles  -- takes parameters as optional second argument.  First
;; argument should be probabilities expressed as fraction
(defmeth binomial-likelihood-family :quantiles
         (prob &key (parameters (send self :default-parameters))
	                 (nui-parameters (send self :nui-default-parameters)))
   (unless (listp prob) (setq prob (list prob)))
   (let ((theta (car parameters))
	 (n (car nui-parameters)))
     (binomial-quant prob n theta)))


;; :atoms  -- takes parameters as keyword argument.  
;; minimum mass as optional third
;; trick use normal approximation to find range of values.
(defmeth binomial-likelihood-family :atoms
         (&key (parameters (send self :default-parameters))
	       (nui-parameters (send self :nui-default-parameters))
	       (min-mass 0.001))
   (let ((theta (car parameters))
	 (n (car nui-parameters)))
     (if (eql 0 min-mass)
	 (iseq 0 n)
       (let ((mu (* n theta))
	     (sigma (sqrt (* n theta (- 1 theta))))
	     (z (abs (normal-quant min-mass))))
	 (apply #'iseq (force2-between
			(list (floor (- mu (* z sigma)))
			      (ceiling (+ mu (* z sigma))))
			(list 0 n)))))))


;; :mass  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth binomial-likelihood-family :mass
         (quant &key (parameters (send self :default-parameters))
		(nui-parameters (send self :nui-default-parameters)))
   (let ((theta (car parameters))
	 (n (car nui-parameters)))
     (binomial-pmf quant n theta)))


;; :cdf  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth binomial-likelihood-family :cdf
         (quant &key (parameters (send self :default-parameters))
		(nui-parameters (send self :nui-default-parameters)))
   (let ((theta (car parameters))
	 (n (car nui-parameters)))
     (binomial-cdf quant n theta)))
	

;;; random variable defaults  (again parameters are optional
;;; arguements)

;; :rv-default  --- default value of rv.

(defmeth binomial-likelihood-family :rv-default
         (&key (parameters (send self :default-parameters))
	       (nui-parameters (send self :nui-default-parameters)))
 (let ((theta (car parameters))
       (n (car nui-parameters)))
   (list (round (* n theta )))))


;; :rv-limits
(defmeth binomial-likelihood-family :rv-limits
         (&key (parameters (send self :default-parameters))
	       (nui-parameters (send self :nui-default-parameters)))
   (list (list 0 *infty*)))

;; :rv-integer?
(defmeth binomial-likelihood-family :rv-integer? (&key &allow-other-keys)
  t)

;; :rv-range-default
(defmeth binomial-likelihood-family :rv-range-default
         (&key (parameters (send self :default-parameters))
	       (nui-parameters (send self :nui-default-parameters)))
   (let ((theta (car parameters))
	 (n (car nui-parameters)))
     (list 
      (force2-between
       (+ (* '(-3 3) (sqrt (* n theta (- 1 theta))))
	  (* n theta))
       (list 0 n)))))

;; :rand
(defmeth binomial-likelihood-family :rand
         (num &key (parameters (send self :default-parameters))
		    (nui-parameters (send self :nui-default-parameters)))
   (let ((theta (car parameters))
	 (n (car nui-parameters)))
     (binomial-rand num n theta)))

;; :mean

(defmeth binomial-likelihood-family :mean
         (&key (parameters (send self :default-parameters))
		    (nui-parameters (send self :nui-default-parameters)))
   (let ((theta (car parameters))
	 (n (car nui-parameters)))
     (list (* theta n))))

;; :variance 
(defmeth binomial-likelihood-family :variance
         (&key (parameters (send self :default-parameters))
		    (nui-parameters (send self :nui-default-parameters)))
  (let ((theta (car parameters))
	(n (car nui-parameters)))
    (list (* n theta (- 1 theta)))))



(send binomial-likelihood-family :isnew :name :binomial-likelihood
      :prior beta-prior-family)

(push binomial-likelihood-family *Known-likelihood-families*)

;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Beta-Binomial Family
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto beta-binomial-family '() '() (list conjugate-family-proto))


;; :isnew method sets up defaults then inherits from conjugate-family-proto
(defmeth beta-binomial-family :isnew
         (&rest args
	  &key  (prior-family beta-prior-family)
	        (likelihood-family binomial-likelihood-family)
	  &allow-other-keys)
 (apply #'call-next-method
	:prior-family prior-family
	:likelihood-family likelihood-family 
	args))


(send beta-binomial-family :isnew)

;;; required link methods for conjugate-families

;;; :forward-link --- returns posterior hyperparameters from prior
;;; hyperparameters 
(defmeth beta-binomial-family :forward-link
  	(&rest args
	 &key  (parameters (send self :default-hyperparameters))
	       (nui-parameters (send self :nui-default-parameters))
	       (data (send self :default-data))
	 &allow-other-keys)
  (let* ((alpha (car parameters))
	 (beta (cadr parameters))
	 (n (car nui-parameters))
	 (X (+ (car data) (- n n)))	;make sure N and X are same shape
	 (sX (if (compound-data-p X) (sum X) X))
	 (sn-X (if (compound-data-p X) (sum (- n X)) (- n X))))
    (list (+ alpha sX) (+ beta sn-X))))


;;; :reverse-link --- returns prior hyperparameters from posterior
;;; hyperparameters 
(defmeth beta-binomial-family :reverse-link
  	(&rest args &key
	       parameters
	       (nui-parameters (send self :nui-parameters-default))
	       (data (send self :default-data))
	       &allow-other-keys)
  (let* ((alpha (car parameters))
	 (beta (cadr parameters))
	 (n (car nui-parameters))
	 (X (+ (car data) (- n n)))	;make sure N and X are same shape
	 (sX (if (compound-data-p X) (sum X) X))
	 (sn-X (if (compound-data-p X) (sum (- n X)) (- n X))))
    (list
     (if (< alpha sX)
		     (progn
		       (warn "beta-binomial:link -alpha, setting to ~S~%"
			     *epsilon*)
		       *epsilon*)
		   (- alpha sX))
     (if (< beta sn-X)
		     (progn
		       (warn "beta-binomial:link -beta, setting to ~S~%"
			     *epsilon*)
		       *epsilon*)
		   (- beta sn-X)))))


(push beta-binomial-family *Known-conjugate-families*)

(new-provide :el-beta-binomial)
