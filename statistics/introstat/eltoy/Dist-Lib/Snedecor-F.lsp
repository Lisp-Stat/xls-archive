;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines F distribution families.

(require :el-family (strcat ElToY-directory "fam" *d-sep*  "family.lsp"))

;;; generic F family  Note that a family is a proto-type which is
;;; sent a :new message to create a specific instance of that family

(defproto F-family '() '() (list continuous-family-proto))

;;; :isnew --- mostly this sets up default args for the call to the
;;; family-proto :isnew
(defmeth F-family :isnew
  	(&rest args
	 &key (name (gensym "F"))
	      (rv-names '(X))
	      (parameter-names '(df-numerator df-denominator))
	      (default-parameters '(3 5))
	      (parameter-range-default '((1 25) (1 50)))
	      (parameter-limits (list (list 0 *infty*)
				      (list 0 *infty*)))
	      (parameter-granularity (list 1 1))
	      (parameter-integer? '(t t))
	      &allow-other-keys)
  (apply #'call-next-method
   :name name
   :rv-names rv-names
   :parameter-names parameter-names
   :default-parameters default-parameters
   :parameter-range-default parameter-range-default
   :parameter-limits parameter-limits
   :parameter-integer? :parameter-integer?
   :parameter-granularity parameter-granularity
   args))



;;; most methods are inherited from family-proto  only those functions
;;; specific to F are maintained.

;; sensible default for print
(send F-family :name :F)

;;; required distribution function methods

;; :quantiles  -- takes parameters as optional second argument.  First
;; argument should be probabilities expressed as fraction
(defmeth F-family :quantiles
         (prob &key (parameters (send self :default-parameters)))
 (let ((df-numerator (car parameters))
       (df-denominator (cadr parameters)))
   (F-quant prob df-numerator df-denominator)))


;; :density  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth F-family :density
         (quant &key (parameters (send self :default-parameters)))
   (let ((df-numerator (car parameters))
	 (df-denominator (cadr parameters)))
     (f-dens quant df-numerator df-denominator)))

;; :cdf  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth F-family :cdf
         (quant &key (parameters (send self :default-parameters)))
   (let ((df-numerator (car parameters))
	 (df-denominator (cadr parameters)))
     (f-cdf quant df-numerator df-denominator)))
	

;;; random variable defaults  (again parameters are optional
;;; arguements)

;; :rv-default  --- default value of rv.

(defmeth F-family :rv-default
         (&key (parameters (send self :default-parameters)))
   (send self :quantiles '(.5) parameters))


;; :rv-limits
(defmeth F-family :rv-limits
         (&key (parameters (send self :default-parameters)))
   (list (list 0 *infty*)))


;; :rv-range-default
(defmeth F-family :rv-range-default
         (&key (parameters (send self :default-parameters)))
   (let ((df-numerator (car parameters))
	 (df-denominator (cadr parameters)))
     (list (list 0 15))))


(defmeth F-family :rand (num &key
			      (parameters (send self :default-parameters)))
  (let ((dfnum (car parameters))
	(dfden (cadr parameters)))
    (F-rand num dfnum dfden)))

(defmeth F-family :mean (&key
			      (parameters (send self :default-parameters)))
  (let ((dfnum (car parameters))
	(dfden (cadr parameters)))
    (if (> df 2)
	(list (/ dfden (- dfden 2))) 
      (progn (warn "F with < 3 df in demonator has no mean~%") nil))))

(defmeth F-family :variance (&key
			      (parameters (send self :default-parameters)))
  (let ((dfnum (car parameters))
	(dfden (cadr parameters)))
    (if (> dfden 4)
	(list (/ (* 2 dfden dfden (+ dfnum dfden -2))
		 (* dfnum (- dfden 2) (- dfden 2) (- dfden 4))))
      (progn (warn "F with < 5 df in demonator has no variance~%") nil))))



;(defmeth F-family :continuous? () t)

(send F-family :isnew :name :F)
(push F-family *Known-families*)

(new-provide :el-F)

