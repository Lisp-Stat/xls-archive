;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines binomial distribution families.

(require :el-family (strcat ElToY-directory "fam" *d-sep*  "family.lsp"))

;;; generic negative binomial family  Note that a family is a proto-type which is
;;; sent a :new message to create a specific instance of that family

(defproto negbinomial-family '() '() (list discrete-family-proto))

;;; :isnew --- mostly this sets up default args for the call to the
;;; family-proto :isnew
(defmeth negbinomial-family :isnew
  	(&rest args
	 &key (name (gensym "NEGATIVE-BINOMIAL"))
	      (rv-names '(X))
	      (parameter-names '(theta r))
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
;;; specific to negbinomial are maintained.

;; sensible default for print
(send negbinomial-family :name :negative-binomial)

;;; required distribution function methods

;; :quantiles
(defmeth negbinomial-family :quantiles
         (prob &key (parameters (send self :default-parameters))
		     (min-mass nil))
   (unless (listp prob) (setq prob (list prob)))
   (let* ((theta (car parameters))
	  (r (cadr parameters))
	  (min-mass (if min-mass min-mass
		      (min (min prob) (min (- 1 prob)) .001)))
	  (atoms (send self :atoms :parameters parameters :min-mass min-mass))
	  (mass (send self :mass atoms :parameters parameters))
	  (cdf (cumsum mass))
	  (pos (mapcar #'(lambda (pr)
			   (position pr cdf :test #'<=))
		       prob)))
     (select atoms pos)))




;; :atoms  -- takes parameters as keyword argument.  
;; minimum mass as optional third
;; trick use normal approximation to find range of values.
(defmeth negbinomial-family :atoms
         (&key (parameters (send self :default-parameters))
	       (min-mass 0.001))
   (let* ((mu (car (send self :mean :parameters parameters)))
	  (sigma (car (send self :variance :parameters parameters)))
	  (z (abs (normal-quant min-mass))))
     (apply #'iseq (list (max 0 (floor (- mu (* z sigma))))
			 (ceiling (+ mu (* 1.25 z sigma)))))))



;; :mass  -- takes parameters as optional second argument.  First
;; argument should be atoms
(defmeth negbinomial-family :mass
         (quant &key (parameters (send self :default-parameters)))
   (let* ((theta (car parameters))
	  (r (cadr parameters))
	  (n (+ r quant)))
     (/ (* (binomial-pmf r n theta) r) n)))


;; :cdf  -- takes parameters as keyed second argument.  First
;; argument should be quantiles
(defmeth negbinomial-family :cdf
         (quant &key (parameters (send self :default-parameters))
		     (min-mass nil))
   (unless (listp quant) (setq quant (list quant)))
   (let* ((theta (car parameters))
	  (r (cadr parameters))
	  (min-mass (if min-mass min-mass
		      (min (min prob) (min (- 1 prob)) .001)))
	  (atoms (send self :atoms :parameters parameters :min-mass min-mass))
	  (mass (send self :mass atoms :parameters parameters))
	  (ranks (rank quant))
	  (oquant (sort-data (quant)))
	  (cmass (if (eql 0 (car atoms)) 0 (/ min-mass 2)))
	  (ocdf '()))
     (dolist (n oquant (select ocdf ranks))
	(let ((watoms (which (>= n atoms))))
	  (setq cmass (+ cmass (sum (select mass watoms))))
	  (setq mass (remove mass watoms))
	  (setq atoms (remove atoms watoms))
	  (push cmass ocdf)))))


(defmeth negbinomial-family :mean
         (&key (parameters (send self :default-parameters)))
   (let* ((theta (car parameters))
	 (r (cadr parameters))
	 (q (- 1 theta)))
     (list (* r (/ q theta )))
     ))

(defmeth negbinomial-family :variance
         (&key (parameters (send self :default-parameters)))
   (let* ((theta (car parameters))
	 (r (cadr parameters))
	 (q (- 1 theta)))
     (list (* r (/ q (* theta theta))))
     ))



;;; random variable defaults  (again parameters are optional
;;; arguements)

;; :rv-default  --- default value of rv.

(defmeth negbinomial-family :rv-default
         (&key (parameters (send self :default-parameters)))
   (round (send self :mean :parameters parameters)))


;; :rv-limits
(defmeth negbinomial-family :rv-limits
         (&key (parameters (send self :default-parameters)))
  (list (list 0 *infty*)))

;; :rv-integer?   
(defmeth negbinomial-family :rv-integer? (&key &allow-other-keys)
  t)
   

;; :rv-range-default
(defmeth negbinomial-family :rv-range-default
         (&key (parameters (send self :default-parameters)))
   (let ((mu (car (send self :mean :parameters parameters)))
	 (sigma (car (send self :variance :parameters parameters)))
	 )
     (apply #'iseq (list (max 0 (floor (- mu (* 3 sigma))))
			 (ceiling (+ mu (* 4.5 sigma)))))))
		    


;(defmeth negbinomial-family :continuous? () nil)

(defmeth negbinomial-family :rand (num &key
			      (parameters (send self :default-parameters)))
  (send self :quantiles (uniform-rand num) :parameters parameters))


(send negbinomial-family :isnew :name :negative-binomial)
(push negbinomial-family *Known-families*)

;
;;; generic geometric family  Note that a family is a proto-type which is
;;; sent a :new message to create a specific instance of that family

(defproto geometric-family '() '() (list discrete-family-proto))

;;; :isnew --- mostly this sets up default args for the call to the
;;; family-proto :isnew
(defmeth geometric-family :isnew
  	(&rest args
	 &key (name (gensym "goemetric"))
	      (rv-names '(X))
	      (parameter-names '(theta))
	      (default-parameters '(.5))
	      (parameter-range-default '((0 1)))
	      (parameter-limits (list (list 0 1)))
	      (parameter-granularity '(.01 ))
	      (parameter-integer? '(nil ))
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
;;; specific to geometric are maintained.

;; sensible default for print
(send geometric-family :name :geometric)

;;; required distribution function methods

;; :quantiles
(defmeth geometric-family :quantiles
         (prob &key (parameters (send self :default-parameters))
		     (min-mass nil))
   (unless (listp prob) (setq prob (list prob)))
   (let* ((theta (car parameters))
	  (r 1)
	  (min-mass (if min-mass min-mass
		      (min (min prob) (min (- 1 prob)) .001)))
	  (atoms (send self :atoms :parameters parameters :min-mass min-mass))
	  (mass (send self :mass atoms :parameters parameters))
	  (cdf (cumsum mass))
	  (pos (mapcar #'(lambda (pr)
			   (position pr cdf :test #'<=))
		       prob)))
     (select atoms pos)))




;; :atoms  -- takes parameters as keyword argument.  
;; minimum mass as optional third
;; trick use normal approximation to find range of values.
(defmeth geometric-family :atoms
         (&key (parameters (send self :default-parameters))
	       (min-mass 0.001))
   (let* ((theta (car parameters)))
     (apply #'iseq (list 1
			 (ceiling (/ (log (/ min-mass theta))
				     (log (- 1 theta))))))))



;; :mass  -- takes parameters as optional second argument.  First
;; argument should be atoms
(defmeth geometric-family :mass
         (quant &key (parameters (send self :default-parameters)))
   (let ((theta (car parameters)))
     (* (** (- 1 theta) (- quant 1)) theta)))


;; :cdf  -- takes parameters as keyed second argument.  First
;; argument should be quantiles
(defmeth geometric-family :cdf
         (quant &key (parameters (send self :default-parameters))
		     (min-mass 0.00001))
   (unless (listp quant) (setq quant (list quant)))
   (let* ((theta (car parameters)))
     (- 1 (** (- 1 theta) quant))))


(defmeth geometric-family :mean
         (&key (parameters (send self :default-parameters)))
   (let* ((theta (car parameters))
	 (q (- 1 theta)))
     (list (* (/ 1 theta )))
     ))

(defmeth geometric-family :variance
         (&key (parameters (send self :default-parameters)))
   (let* ((theta (car parameters))
	 (q (- 1 theta)))
     (list (/ q (* theta theta)))
     ))



;;; random variable defaults  (again parameters are optional
;;; arguements)

;; :rv-default  --- default value of rv.

;(defmeth geometric-family :rv-default
;         (&key (parameters (send self :default-parameters)))
;   (round (send self :mean :parameters parameters)))


;; :rv-limits
(defmeth geometric-family :rv-limits
         (&key (parameters (send self :default-parameters)))
  (list (list 1 *infty*)))
   
;; :rv-integer?   
(defmeth geometric-family :rv-integer? (&key &allow-other-keys)
  t)

;; :rv-range-default
(defmeth geometric-family :rv-range-default
         (&key (parameters (send self :default-parameters)))
   (let ((mu (car (send self :mean :parameters parameters))) 
	 (sigma (car (send self :variance :parameters parameters)))
	 )
     (apply #'iseq (list 1 (ceiling (+ mu (* 4.5 sigma)))))))
		    


;(defmeth geometric-family :continuous? () nil)

(defmeth geometric-family :rand (num &key
			      (parameters (send self :default-parameters)))
  (send self :quantiles (uniform-rand num) :parameters parameters))


(send geometric-family :isnew :name :geometric)
(push geometric-family *Known-families*)

(new-provide :el-negbinomial)

