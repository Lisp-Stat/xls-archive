;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines normal distribution families.

(require :el-family (strcat ElToY-directory "fam" *d-sep*  "family.lsp"))

;;; generic normal family  Note that a family is a proto-type which is
;;; sent a :new message to create a specific instance of that family

(defproto normal-family '() '() (list continuous-family-proto))

;;; :isnew --- mostly this sets up default args for the call to the
;;; family-proto :isnew
(defmeth normal-family :isnew
  	(&rest args
	 &key (name (gensym "NORMAL"))
	      (rv-names '(X))
	      (parameter-names '(Mean StDev))
	      (default-parameters '(0 1))
	      (parameter-range-default '((-10 10) (0 5)))
	      (parameter-limits (list (list *-infty* *infty*)
				      (list 0 *infty*)))
	      (parameter-granularity (list .25 .1))
	      (parameter-integer? '(nil nil))
	      &allow-other-keys)
  (apply #'call-next-method
   :name name
   :rv-names rv-names
   :parameter-names parameter-names
   :default-parameters default-parameters
   :parameter-range-default parameter-range-default
   :parameter-limits parameter-limits
   :parameter-integer? parameter-integer?
   :parameter-granularity parameter-granularity
   args))



;;; most methods are inherited from family-proto  only those functions
;;; specific to normal are maintained.

;; sensible default for print
(send normal-family :name :normal0)

;;; required distribution function methods

;; :quantiles  -- takes parameters as optional second argument.  First
;; argument should be probabilities expressed as fraction
(defmeth normal-family :quantiles
         (prob &key (parameters (send self :default-parameters)))
  (unless (listp prob) (setq prob (list prob)))
  (let ((mean (car parameters))
	(stdev (cadr parameters)))
    (+ mean (* stdev (normal-quant prob)))))


;; :density  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth normal-family :density
         (quant &key (parameters (send self :default-parameters)))
   (let ((mean (car parameters))
	 (stdev (cadr parameters)))
     (/ (normal-dens (/ (- quant mean) stdev))
	stdev)))

;; :cdf  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth normal-family :cdf
         (quant &key (parameters (send self :default-parameters)))
   (let ((mean (car parameters))
	 (stdev (cadr parameters)))
     (normal-cdf (/ (- quant mean) stdev))))
	

;;; random variable defaults  (again parameters are optional
;;; arguements)

;; :rv-default  --- default value of rv.

(defmeth normal-family :rv-default
         (&key (parameters (send self :default-parameters)))
   (list (car parameters)))		;mean


;; :rv-limits
(defmeth normal-family :rv-limits
         (&key (parameters (send self :default-parameters)))
   (list (list *-infty* *infty*)))


;; :rv-range-default
(defmeth normal-family :rv-range-default
         (&key (parameters (send self :default-parameters)))
   (let ((mean (car parameters))
	 (stdev (cadr parameters)))
     (list (+ (* '(-3 3) stdev) mean))))


;(defmeth normal-family :continuous? () t)

(defmeth normal-family :rand (num &key
			      (parameters (send self :default-parameters)))
  (let ((mean (car parameters))
	(stdev (cadr parameters)))
    (+ (* (normal-rand num) stdev) mean)))

(defmeth normal-family :mean (&key
			      (parameters (send self :default-parameters)))
  (let ((mean (car parameters))
	(stdev (cadr parameters)))
    (list mean)))

(defmeth normal-family :variance (&key
			      (parameters (send self :default-parameters)))
  (let ((mean (car parameters))
	(stdev (cadr parameters)))
    (list (* stdev stdev))))


(send normal-family :isnew :name :normal)
(push normal-family *Known-families*)


(new-provide :el-normal)

