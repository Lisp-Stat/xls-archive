;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines binomial distribution families.


(require :el-family (strcat ElToY-directory "fam" *d-sep*  "family.lsp"))

;;; generic binomial family  Note that a family is a proto-type which is
;;; sent a :new message to create a specific instance of that family

(defproto binomial-family '() '() (list discrete-family-proto))

;;; :isnew --- mostly this sets up default args for the call to the
;;; family-proto :isnew
(defmeth binomial-family :isnew
  	(&rest args
	 &key (name (gensym "BINOMIAL"))
	      (rv-names '(X))
	      (parameter-names '(theta n))
	      (default-parameters '(.5 10))
	      (parameter-range-default '((0 1) (1 25)))
	      (parameter-limits (list (list 0 1)
				      (list 1 *infty*)))
	      (parameter-granularity '(.01 1))
	      (parameter-integer? '(nil t))
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
;;; specific to binomial are maintained.

;; sensible default for print
(send binomial-family :name :binomial)

;;; required distribution function methods

;; :quantiles
(defmeth binomial-family :quantiles
         (prob &key (parameters (send self :default-parameters)))
  (unless (listp prob) (setq prob (list prob)))
  (let ((theta (car parameters))
	(n (cadr parameters)))
    (binomial-quant prob theta n)))


;; :atoms  -- takes parameters as keyword argument.  
;; minimum mass as optional third
;; trick use normal approximation to find range of values.
(defmeth binomial-family :atoms
         (&key (parameters (send self :default-parameters))
	       (min-mass 0.001))
   (let ((theta (car parameters))
	 (n (cadr parameters)))
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
;; argument should be atoms
(defmeth binomial-family :mass
         (quant &key (parameters (send self :default-parameters)))
   (let ((theta (car parameters))
	 (n (cadr parameters)))
     (binomial-pmf quant n theta)))


;; :cdf  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth binomial-family :cdf
         (quant &key (parameters (send self :default-parameters)))
   (let ((theta (car parameters))
	 (n (cadr parameters)))
     (binomial-cdf quant n theta)))


;;; random variable defaults  (again parameters are optional
;;; arguements)

;; :rv-default  --- default value of rv.

(defmeth binomial-family :rv-default
         (&key (parameters (send self :default-parameters)))
   (let ((theta (car parameters))
	 (n (cadr parameters)))
     (list (round (* n theta )))))


;; :rv-limits
(defmeth binomial-family :rv-limits
         (&key (parameters (send self :default-parameters)))
   (let ((n (cadr parameters)))
     (list (list 0 n))))

;; :rv-integer?   
(defmeth binomial-family :rv-integer? (&key &allow-other-keys)
  t)

;; :rv-range-default
(defmeth binomial-family :rv-range-default
         (&key (parameters (send self :default-parameters)))
   (let ((theta (car parameters))
	 (n (cadr parameters)))
     (force-between
      (list (+ (* '(-3 3) (sqrt (* n theta (- 1 theta))))
	       (* n theta)))
      (list 0 n))))


;(defmeth binomial-family :continuous? () nil)

(defmeth binomial-family :rand (num &key
			      (parameters (send self :default-parameters)))
  (let ((theta (car parameters))
	 (n (cadr parameters)))
    (binomial-rand num n theta)))

(defmeth binomial-family :mean (&key
			      (parameters (send self :default-parameters)))
  (let ((theta (car parameters))
	(n (cadr parameters)))
    (list (* n theta))))

(defmeth binomial-family :variance (&key
			      (parameters (send self :default-parameters)))
  (let ((theta (car parameters))
	 (n (cadr parameters)))
    (list (* n theta (- 1 theta)))))


(send binomial-family :isnew :name :binomial)
(push binomial-family *Known-families*)

(new-provide :el-binomial)

