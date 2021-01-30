;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines Gamma distribution families.

(require :el-family (strcat ElToY-directory "fam" *d-sep*  "family.lsp"))

;;; generic Gamma family  Note that a family is a proto-type which is
;;; sent a :new message to create a specific instance of that family

(defproto gamma-family '() '() (list continuous-family-proto))

;;; :isnew --- mostly this sets up default args for the call to the
;;; family-proto :isnew
(defmeth gamma-family :isnew
  	(&rest args
	 &key (name (gensym "GAMMA"))
	      (rv-names '(X))
	      (parameter-names '(Shape Scale))
	      (default-parameters '(2 1))
	      (parameter-range-default '((0 20) (0 5)))
	      (parameter-limits (list (list 0 *infty*)
				      (list 0 *infty*)))
	      (parameter-granularity (list 1 .25))
	      (parameter-integer? (list nil nil))
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
;;; specific to gamma are maintained.

;; sensible default for print
(send gamma-family :name :gamma)

;;; required distribution function methods

;; :quantiles  -- takes parameters as optional second argument.  First
;; argument should be probabilities expressed as fraction
(defmeth gamma-family :quantiles
         (prob &key (parameters (send self :default-parameters)))
  (unless (listp prob) (setq prob (list prob)))
  (let ((shape (car parameters))
	(scale (cadr parameters)))
    (* scale (gamma-quant prob shape))))


;; :density  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth gamma-family :density
         (quant &key (parameters (send self :default-parameters)))
   (let ((shape (car parameters))
	 (scale (cadr parameters)))
     (/ (gamma-dens (/ quant scale) shape)
	scale)))

;; :cdf  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth gamma-family :cdf
         (quant &key (parameters (send self :default-parameters)))
   (let ((shape (car parameters))
	 (scale (cadr parameters)))
     (gamma-cdf (/ quant scale) shape)))
	

;;; random variable defaults  (again parameters are optional
;;; arguements)

;; :rv-default  --- default value of rv.

(defmeth gamma-family :rv-default
         (&key (parameters (send self :default-parameters)))
   (send self :quantiles '(.5) parameters))


;; :rv-limits
(defmeth gamma-family :rv-limits
         (&key (parameters (send self :default-parameters)))
   (list (list 0 *infty*)))


;; :rv-range-default
(defmeth gamma-family :rv-range-default
         (&key (parameters (send self :default-parameters)))
   (let ((shape (car parameters))
	 (scale (cadr parameters)))
     (list (* '(0 5) scale))))


(defmeth gamma-family :rand (num &key
			      (parameters (send self :default-parameters)))
  (let ((shape (car parameters))
	(scale (cadr parameters)))
    (* (gamma-rand num shape) scale)))

(defmeth gamma-family :mean (&key
			      (parameters (send self :default-parameters)))
  (let ((shape (car parameters))
	(scale (cadr parameters)))
    (list (* shape scale))))

(defmeth gamma-family :variance (&key
			      (parameters (send self :default-parameters)))
  (let ((shape (car parameters))
	(scale (cadr parameters)))
    (list (* shape scale scale))))

;(defmeth gamma-family :continuous? () t)


(send gamma-family :isnew :name :gamma)
(push gamma-family *Known-families*)


;
;;; --------------- Exponential Family ---------------

(defproto exponential-family '() '() (list continuous-family-proto))


(defmeth exponential-family :isnew
  	(&rest args
	 &key (name (gensym "Exponential"))
	      (rv-names '(X))
	      (parameter-names '(Scale))
	      (default-parameters '(1))
	      (parameter-range-default '((0 5)))
	      (parameter-limits (list (list 0 *infty*)))
	      (parameter-granularity (list .25))
	      (parameter-integer? (list nil))
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

;; sensible default for print
(send exponential-family :name :exponential)

;;; required distribution function methods

;; :quantiles  -- takes parameters as optional second argument.  First
;; argument should be probabilities expressed as fraction
(defmeth exponential-family :quantiles
         (prob &key (parameters (send self :default-parameters)))
  (unless (listp prob) (setq prob (list prob)))
  (let ((shape 1)
	(scale (car parameters)))
    (* scale (gamma-quant prob shape))))


;; :density  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth exponential-family :density
         (quant &key (parameters (send self :default-parameters)))
   (let ((shape 1)
	 (scale (car parameters)))
     (/ (gamma-dens (/ quant scale) shape)
	scale)))

;; :cdf  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth exponential-family :cdf
         (quant &key (parameters (send self :default-parameters)))
   (let ((shape 1)
	 (scale (car parameters)))
     (gamma-cdf (/ quant scale) shape)))
	

;;; random variable defaults  (again parameters are optional
;;; arguements)

;; :rv-default  --- default value of rv.

(defmeth exponential-family :rv-default
         (&key (parameters (send self :default-parameters)))
   (send self :quantiles '(.5) parameters))


;; :rv-limits
(defmeth exponential-family :rv-limits
         (&key (parameters (send self :default-parameters)))
   (list (list 0 *infty*)))


;; :rv-range-default
(defmeth exponential-family :rv-range-default
         (&key (parameters (send self :default-parameters)))
   (let ((shape 1)
	 (scale (car parameters)))
     (list (* '(0 5) scale))))


(defmeth exponential-family :rand (num &key
			      (parameters (send self :default-parameters)))
  (let ((shape 1)
	(scale (car parameters)))
    (* (gamma-rand num shape) scale)))

(defmeth exponential-family :mean (&key
			      (parameters (send self :default-parameters)))
  (let ((shape 1)
	(scale (car parameters)))
    (list (* shape scale))))

(defmeth exponential-family :variance (&key
			      (parameters (send self :default-parameters)))
  (let ((shape 1)
	(scale (car parameters)))
    (list (* shape scale scale))))

;(defmeth exponential-family :continuous? () t)


(send exponential-family :isnew :name :exponential)
(push exponential-family *Known-families*)

(new-provide :el-gamma)



