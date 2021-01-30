;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines Chisq distribution families.

(require :el-family (strcat ElToY-directory "fam" *d-sep*  "family.lsp"))

;;; generic Chisq family  Note that a family is a proto-type which is
;;; sent a :new message to create a specific instance of that family

(defproto chisq-family '() '() (list continuous-family-proto))

;;; :isnew --- mostly this sets up default args for the call to the
;;; family-proto :isnew
(defmeth chisq-family :isnew
  	(&rest args
	 &key (name (gensym "CHISQ"))
	      (rv-names '(X))
	      (parameter-names '(d.f. Scale))
	      (default-parameters '(3 1))
	      (parameter-range-default '((1 20) (0 5)))
	      (parameter-limits (list (list 0 *infty*)
				      (list 0 *infty*)))
	      (parameter-granularity (list 1 .1))
	      (parameter-integer? (list t nil))
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
;;; specific to chisq are maintained.

;; sensible default for print
(send chisq-family :name :chisq)

;;; required distribution function methods

;; :quantiles  -- takes parameters as optional second argument.  First
;; argument should be probabilities expressed as fraction
(defmeth chisq-family :quantiles
         (prob &key (parameters (send self :default-parameters)))
  (unless (listp prob) (setq prob (list prob)))
  (let ((df (car parameters))
	(scale (cadr parameters)))
    (* scale (chisq-quant prob df))))


;; :density  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth chisq-family :density
         (quant &key (parameters (send self :default-parameters)))
   (let ((df (car parameters))
	 (scale (cadr parameters)))
     (/ (chisq-dens (/ quant scale) df)
	scale)))

;; :cdf  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth chisq-family :cdf
         (quant &key (parameters (send self :default-parameters)))
   (let ((df (car parameters))
	 (scale (cadr parameters)))
     (chisq-cdf (/ quant scale) df)))
	

;;; random variable defaults  (again parameters are optional
;;; arguements)

;; :rv-default  --- default value of rv.

(defmeth chisq-family :rv-default
         (&key (parameters (send self :default-parameters)))
   (send self :quantiles '(.5) parameters))


;; :rv-limits
(defmeth chisq-family :rv-limits
         (&key (parameters (send self :default-parameters)))
   (list (list 0 *infty*)))


;; :rv-range-default
(defmeth chisq-family :rv-range-default
         (&key (parameters (send self :default-parameters)))
   (let ((df (car parameters))
	 (scale (cadr parameters)))
     (list (* '(0 5) scale))))

(defmeth chisq-family :rand (num &key
			      (parameters (send self :default-parameters)))
  (let ((df (car parameters))
	(scale (cadr parameters)))
    (* (chisq-rand num df) scale)))

(defmeth chisq-family :mean (&key
			      (parameters (send self :default-parameters)))
  (let ((df (car parameters))
	(scale (cadr parameters)))
    (list (*  df scale))))


(defmeth chisq-family :variance (&key
			      (parameters (send self :default-parameters)))
  (let ((df (car parameters))
	(scale (cadr parameters)))
    (list (*  2 df scale scale))))


;(defmeth chisq-family :continuous? () t)

(send chisq-family :isnew :name :chisq)
(push chisq-family *Known-families*)

(new-provide :el-chisq)



