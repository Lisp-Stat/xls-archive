;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines cauchy distribution families.

(require :el-family (strcat ElToY-directory "fam" *d-sep*  "family.lsp"))

;;; generic cauchy family  Note that a family is a proto-type which is
;;; sent a :new message to create a specific instance of that family

(defproto cauchy-family '() '() (list continuous-family-proto))

;;; :isnew --- mostly this sets up default args for the call to the
;;; family-proto :isnew
(defmeth cauchy-family :isnew
  	(&rest args
	 &key (name (gensym "CAUCHY"))
	      (rv-names '(X))
	      (parameter-names '(Location Scale))
	      (default-parameters '(0 1))
	      (parameter-range-default '((-10 10) (0 5)))
	      (parameter-limits (list (list *-infty* *infty*)
				      (list 0 *infty*)))
	      (parameter-granularity (list .25 .1))
	      (parameter-integer? '( nil nil))
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
;;; specific to cauchy are maintained.

;; sensible default for print
(send cauchy-family :name :cauchy)

;;; required distribution function methods

;; :quantiles  -- takes parameters as optional second argument.  First
;; argument should be probabilities expressed as fraction
(defmeth cauchy-family :quantiles
         (prob &key (parameters (send self :default-parameters)))
  (unless (listp prob) (setq prob (list prob)))
  (let ((location (car parameters))
	(scale (cadr parameters)))
    (+ location (* scale (cauchy-quant prob)))))


;; :density  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth cauchy-family :density
         (quant &key (parameters (send self :default-parameters)))
   (let ((location (car parameters))
	 (scale (cadr parameters)))
     (/ (cauchy-dens (/ (- quant location) scale))
	scale)))

;; :cdf  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth cauchy-family :cdf
         (quant &key (parameters (send self :default-parameters)))
   (let ((location (car parameters))
	 (scale (cadr parameters)))
     (cauchy-cdf (/ (- quant location) scale))))
	

;;; random variable defaults  (again parameters are optional
;;; arguements)

;; :rv-default  --- default value of rv.

(defmeth cauchy-family :rv-default
         (&key (parameters (send self :default-parameters)))
   (list (car parameters)))		;location parameter


;; :rv-limits
(defmeth cauchy-family :rv-limits
         (&key (parameters (send self :default-parameters)))
   (list (list *-infty* *infty*)))


;; :rv-range-default
(defmeth cauchy-family :rv-range-default
         (&key (parameters (send self :default-parameters)))
   (let ((location (car parameters))
	 (scale (cadr parameters)))
     (list (+ (* '(-3 3) scale) location))))


(defmeth cauchy-family :rand (num &key
			      (parameters (send self :default-parameters)))
  (let ((location (car parameters))
	(scale (cadr parameters)))
    (+ (* (normal-rand num) scale) location)))

(defmeth cauchy-family :mean (&key
			      (parameters (send self :default-parameters)))
  (warn "Cauchy distribution does not have mean")
  nil)

(defmeth cauchy-family :variance (&key
			      (parameters (send self :default-parameters)))
  (warn "Cauchy distribution does not have variance")
  nil)


;(defmeth cauchy-family :continuous? () t)

(send cauchy-family :isnew :name :cauchy)
(push cauchy-family *Known-families*)


(new-provide :el-cauchy)
