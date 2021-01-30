;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines Poisson distribution families.

(require :el-family (strcat ElToY-directory "fam" *d-sep*  "family.lsp"))

;;; generic Poisson family  Note that a family is a proto-type which is
;;; sent a :new message to create a specific instance of that family

(defproto Poisson-family '() '() (list discrete-family-proto))

;;; :isnew --- mostly this sets up default args for the call to the
;;; family-proto :isnew
(defmeth Poisson-family :isnew
  	(&rest args
	 &key (name (gensym "POISSON"))
	      (rv-names '(X))
	      (parameter-names '(lambda))
	      (default-parameters '(10))
	      (parameter-range-default '((1 25)))
	      (parameter-limits (list (list 0 *infty*)))
	      (parameter-granularity '(.5))
	      (parameter-integer? '(nil))
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
;;; specific to Poisson are maintained.

;; sensible default for print
(send Poisson-family :name :Poisson)

;;; required distribution function methods


;; :quantiles
(defmeth Poisson-family :quantiles
         (prob &key (parameters (send self :default-parameters)))
   (let ((theta (car parameters)))
     (Poisson-quant prob theta)))



;; :atoms  -- takes parameters as keyword argument.  
;; minimum mass as optional third

;; trick, use normal approximation to find range of atoms
(defmeth Poisson-family :atoms
         (&key (parameters (send self :default-parameters))
	       (min-mass 0.001))
   (let* ((lambda (car parameters))
	  (sigma (sqrt lambda))
	  (z (normal-quant (- 1 min-mass))))
     (iseq (max 0 (floor (- lambda (* z sigma))))
	   (ceiling (+ lambda (* z sigma))))))


;; :mass  -- takes parameters as optional second argument.  First
;; argument should be atoms
(defmeth Poisson-family :mass
         (quant &key (parameters (send self :default-parameters)))
  (let ((lambda (car parameters)))
    (Poisson-pmf quant lambda)))


;; :cdf  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth Poisson-family :cdf
         (quant &key (parameters (send self :default-parameters)))
   (let ((lambda (car parameters)))
     (Poisson-cdf quant lambda)))


;;; random variable defaults  (again parameters are optional
;;; arguements)

;; :rv-default  --- default value of rv.

(defmeth Poisson-family :rv-default
         (&key (parameters (send self :default-parameters)))
   (let ((lambda (car parameters)))
     (list (round lambda))))


;; :rv-limits
(defmeth Poisson-family :rv-limits
         (&key (parameters (send self :default-parameters)))
   (let ((lambda (car parameters)))
     (list (list 0 *infty*))))

;; :rv-integer?   
(defmeth Poisson-family :rv-integer? (&key &allow-other-keys)
  t)


;; :rv-range-default
(defmeth Poisson-family :rv-range-default
         (&key (parameters (send self :default-parameters)))
   (let ((lambda (car parameters)))
     (force-between
      (list (+ (* '(-3 3) (sqrt lambda)) lambda)
	    (list 0 (* 10 lambda))))))


;(defmeth Poisson-family :continuous? () nil)

(defmeth Poisson-family :rand (num &key
			      (parameters (send self :default-parameters)))
  (let ((lambda (car parameters)))
    (Poisson-rand num lambda)))

(defmeth Poisson-family :mean (&key
			      (parameters (send self :default-parameters)))
  (let ((lambda (car parameters)))
    (list lambda)))

(defmeth Poisson-family :variance (&key
			      (parameters (send self :default-parameters)))
  (let ((lambda (car parameters)))
    (list lambda)))


(send Poisson-family :isnew :name :Poisson)
(push Poisson-family *Known-families*)

(new-provide :el-Poisson)

