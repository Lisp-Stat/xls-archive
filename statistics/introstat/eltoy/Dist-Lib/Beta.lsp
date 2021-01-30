;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines beta distribution families.

(require :el-family (strcat ElToY-directory "fam" *d-sep*  "family.lsp"))

;;; generic beta family  Note that a family is a proto-type which is
;;; sent a :new message to create a specific instance of that family

(defproto beta-family '() '() (list continuous-family-proto))

;;; :isnew --- mostly this sets up default args for the call to the
;;; family-proto :isnew
(defmeth beta-family :isnew
  	(&rest args
	 &key (name (gensym "BETA"))
	      (rv-names '(X))
	      (parameter-names '(alpha beta))
	      (default-parameters '(1 1))
	      (parameter-range-default '((0.5 50) (0.5 50)))
	      (parameter-limits (list (list 0 *infty*)
				      (list 0 *infty*)))
	      (parameter-granularity (list 0.5 0.5))
	      (parameter-integer? `(nil nil))
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
;;; specific to beta are maintained.

;; sensible default for print
(send beta-family :name :beta)

;;; required distribution function methods

;; :quantiles  -- takes parameters as optional second argument.  First
;; argument should be probabilities expressed as fraction
(defmeth beta-family :quantiles
         (prob &key (parameters (send self :default-parameters)))
 (unless (listp prob) (setq prob (list prob)))
 (let ((alpha (car parameters))
       (beta (cadr parameters)))
   (beta-quant prob alpha beta)))


;; :density  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth beta-family :density
         (quant &key (parameters (send self :default-parameters)))
   (let ((alpha (car parameters))
	 (beta (cadr parameters)))
     (beta-dens quant alpha beta)))

;; :cdf  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth beta-family :cdf
         (quant &key (parameters (send self :default-parameters)))
   (let ((alpha (car parameters))
	 (beta (cadr parameters)))
     (beta-cdf quant alpha beta)))
	

;;; random variable defaults  (again parameters are optional
;;; arguements)

;; :rv-default  --- default value of rv.

(defmeth beta-family :rv-default
         (&key (parameters (send self :default-parameters)))
   (send self :quantiles '(.5) parameters))


;; :rv-limits
(defmeth beta-family :rv-limits
         (&key (parameters (send self :default-parameters)))
   (list (list 0 1)))


;; :rv-range-default
(defmeth beta-family :rv-range-default
         (&key (parameters (send self :default-parameters)))
   (let ((alpha (car parameters))
	 (beta (cadr parameters)))
     (list (list 0 1))))


(defmeth beta-family :rand (num &key
				(parameters (send self :default-parameters)))
  (let ((alpha (car parameters))
	(beta (cadr parameters)))
    (beta-rand num alpha beta)))

(defmeth beta-family :mean (&key
				(parameters (send self :default-parameters)))
  (let ((alpha (car parameters))
	(beta (cadr parameters)))
    (list (/ alpha (+ alpha beta)))))

(defmeth beta-family :variance (&key
				(parameters (send self :default-parameters)))
  (let* ((alpha (car parameters))
	(beta (cadr parameters))
	(nu (+ alpha beta)))
    (list (/ (* alpha beta) (* nu nu (1+ nu))))))


;(defmeth beta-family :continuous? () t)

(send beta-family :isnew :name :beta)
(push beta-family *Known-families*)

(new-provide :el-beta)
