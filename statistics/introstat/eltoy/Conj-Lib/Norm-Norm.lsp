;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines normal distribution families.

(require :el-conjugate (strcat ElToY-directory "conj" *d-sep*  "conjugate.lsp"))
(require :el-normal (strcat ElToY-directory "Dist-Lib" *d-sep*  "Normal.lsp"))



;;;;;; -------------------   Normal Prior Distribution ----------------
;; Meant to be used as prior in a normal-normal family

(defproto normal-prior-family () () (list normal-family continuous-prior-proto)
  "Normal prior distribution")


;;; depends on the normal-family proto :isnew method heavily
(defmeth normal-prior-family :isnew
  	(&rest args
	 &key (name (gensym "NORMAL-P-"))
	      (rv-names '(theta))
	      (parameter-names '(mu tau))
	      (default-parameters '(0 100))
	      (parameter-range-default '((-10 10) (0 100)))
	      (parameter-granularity '(.25 2))
	      &allow-other-keys)
  (apply #'call-next-method
   :name name
   :rv-names rv-names
   :parameter-names parameter-names
   :default-parameters default-parameters
   :parameter-range-default parameter-range-default
   :parameter-granularity parameter-granularity
   args))


(send normal-prior-family :name :normal-prior)
(send normal-prior-family :isnew :name :normal-prior)

(push normal-prior-family *Known-prior-families*)

;
;;;;;; Normal Likelihood Distribution
;; Meant to be used as prior in a normal-normal family

(defproto normal-likelihood-family () () (list continuous-likelihood-proto) 
  "Normal likelihood distribution")


;;; depends on the normal-family proto :isnew method heavily
(defmeth normal-likelihood-family :isnew
  	(&rest args
	 &key (prior)
	      (name (gensym "NORMAL-L-"))
	      (rv-names '(X))
	      (nui-parameter-names '(sigma))
	      (nui-default-parameters '(1))
	      (nui-parameter-range-default '((0 5)))
	      (nui-parameter-integer? '(nil))
	      (nui-parameter-granularity '(.1))
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
   args))



(send normal-likelihood-family :name :normal-likelihood)


;;; required distribution function methods

;; :quantiles  -- takes parameters as optional second argument.  First
;; argument should be probabilities expressed as fraction
(defmeth normal-likelihood-family :quantiles
         (prob &key (parameters (send self :default-parameters))
	                 (nui-parameters (send self :nui-default-parameters)))
   (unless (listp prob) (setq prob (list prob)))
   (let ((mean (car parameters))
	 (stdev (car nui-parameters)))
     (+ mean (* stdev (normal-quant prob)))))


;; :density  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth normal-likelihood-family :density
         (quant &key (parameters (send self :default-parameters))
		          (nui-parameters (send self :nui-default-parameters)))
   (let ((mean (car parameters))
	 (stdev (car nui-parameters)))
     (/ (normal-dens (/ (- quant mean) stdev))
	stdev)))

;; :cdf  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth normal-likelihood-family :cdf
         (quant &key (parameters (send self :default-parameters))
		          (nui-parameters (send self :nui-default-parameters)))
   (let ((mean (car parameters))
	 (stdev (car nui-parameters)))
     (normal-cdf (/ (- quant mean) stdev))))
	

;;; random variable defaults  (again parameters are optional
;;; arguements)

;; :rv-default  --- default value of rv.

(defmeth normal-likelihood-family :rv-default
         (&key (parameters (send self :default-parameters))
		    (nui-parameters (send self :nui-default-parameters)))
   (list (car parameters)))			;mean


;; :rv-limits
(defmeth normal-likelihood-family :rv-limits
         (&key (parameters (send self :default-parameters))
		    (nui-parameters (send self :nui-default-parameters)))
   (list (list *-infty* *infty*)))


;; :rv-range-default
(defmeth normal-likelihood-family :rv-range-default
         (&key (parameters (send self :default-parameters))
		    (nui-parameters (send self :nui-default-parameters)))
   (let ((mean (car parameters))
	 (stdev (car nui-parameters)))
     (list (+ (* '(-3 3) stdev) mean))))

;; :rand
(defmeth normal-likelihood-family :rand
         (num &key (parameters (send self :default-parameters))
		    (nui-parameters (send self :nui-default-parameters)))
   (let ((mean (car parameters))
	 (stdev (car nui-parameters)))
     (+ (* (normal-rand num) stdev) mean)))

;; :mean

(defmeth normal-likelihood-family :mean
         (&key (parameters (send self :default-parameters))
		    (nui-parameters (send self :nui-default-parameters)))
   (let ((mean (car parameters)))
     (list mean)))

;; :variance 
(defmeth normal-likelihood-family :variance
         (&key (parameters (send self :default-parameters))
		    (nui-parameters (send self :nui-default-parameters)))
   (let ((stdev (car nui-parameters)))
     (list (* stdev stdev))))


(send normal-likelihood-family :isnew :name :normal-likelihood
      :prior normal-prior-family)

(push normal-likelihood-family *Known-likelihood-families*)

;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Normal-Normal Family
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto normal-normal-family '() '() (list conjugate-family-proto))


;; :isnew method sets up defaults then inherits from conjugate-family-proto
(defmeth normal-normal-family :isnew
         (&rest args
	  &key  (prior-family normal-prior-family)
	        (likelihood-family normal-likelihood-family)
	  &allow-other-keys)
 (apply #'call-next-method
	:prior-family prior-family
	:likelihood-family likelihood-family 
	args))


(send normal-normal-family :isnew)

;;; required link methods for conjugate-families

;;; :forward-link --- returns posterior hyperparameters from prior
;;; hyperparameters 
(defmeth normal-normal-family :forward-link
  	(&rest args
	 &key  (parameters (send self :default-hyperparameters))
	       (nui-parameters (send self :nui-default-parameters))
	       (data (send self :default-data))
	 &allow-other-keys)
  (let* ((mu (car parameters))
	 (tau-2 (/ 1.0 (* (cadr parameters) (cadr parameters))))
	 (X (car data))
	 (sigma-2 (+ (/ 1.0 (* (car nui-parameters) (car nui-parameters)))
		     (- X X)))
					;makes the same shape as X
	 (tau-2* (+ tau-2 (sum sigma-2)))
	 (mu* (/ (+ (* tau-2 mu) (sum (* X sigma-2))) tau-2*)))
    (list mu* (sqrt (/ 1.0 tau-2*)))))


;;; :reverse-link --- returns posterior hyperparameters from prior
;;; hyperparameters 
(defmeth normal-normal-family :reverse-link
  	(&rest args &key
	       parameters
	       (nui-parameters (send self :nui-parameters-default))
	       (data (send self :default-data))
	       &allow-other-keys)
  (let* ((mu* (car parameters))
	 (tau-2* (/ 1.0 (* (cadr parameters) (cadr parameters))))
	 (X (car data))
	 (sigma-2 (+ (/ 1.0 (* (car nui-parameters) (car nui-parameters)))
		     (- X X)))
	 (tau-2 (max *epsilon* (- tau-2* (sum sigma-2))))
	 (mu (/ (- (* tau-2* mu*) (sum (* X sigma-2))) tau-2)))
    (if (<= tau-2 0) (uerror "force to *epsilon*"
			     "Negative variance mu*: ~S tau-2*:~A X:~S
sigma-2: ~S tau-2: ~S mu: ~S~%" mu* tau-2* X sigma-2 tau-2 mu))
    (list mu (sqrt (/ 1.0 tau-2)))))


(push normal-normal-family *Known-conjugate-families*)

(new-provide :el-norm-norm)
