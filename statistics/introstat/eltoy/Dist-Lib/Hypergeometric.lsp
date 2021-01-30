;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines Hypergeometric distribution families.

(require :el-family (strcat ElToY-directory "fam" *d-sep*  "family.lsp"))

;;; generic hypergeometric family  Note that a family is a proto-type which is
;;; sent a :new message to create a specific instance of that family

(defproto hypergeometric-family '() '() (list discrete-family-proto))

;;; :isnew --- mostly this sets up default args for the call to the
;;; family-proto :isnew
(defmeth hypergeometric-family :isnew
  	(&rest args
	 &key (name (gensym "HYPERGEOMETRIC"))
	      (rv-names '(X))
	      (parameter-names '(r b n))
	      (default-parameters '(5 5 3))
	      (parameter-range-default '((0 25) (0 25) (1 10)))
	      (parameter-limits (list (list 0 *infty*) (list 0 *infty*)
				      (list 1 *infty*)))
	      (parameter-granularity '(1 1 1))
	      (parameter-integer? '(t t t))
	      (parameter-constraint-fun #'hypergeometric-constr-fun)
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
   :parameter-constraint-fun parameter-constraint-fun
   args))



;; constraint function for parameters
(defun hypergeometric-constr-fun (parameters &key (warn t)
					     &allow-other-keys)
  (let ((r (car parameters))
	(b (cadr parameters))
	(k (caddr parameters)))
    (cond ((> k (+ r b))
	   (if warn (warn "More samples (~A) than balls in urn: ~A + ~A~%"
			  k r b))
	   nil)
	  (t t))))


;;; most methods are inherited from family-proto  only those functions
;;; specific to hypergeometric are maintained.

;; sensible default for print
(send hypergeometric-family :name :hypergeometric)

;;; required distribution function methods

;; :quantiles
(defmeth hypergeometric-family :quantiles
         (prob &key (parameters (send self :default-parameters))
		     (min-mass nil))
   (unless (listp prob) (setq prob (list prob)))
   (let* ((min-mass (if min-mass min-mass
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
(defmeth hypergeometric-family :atoms
         (&key (parameters (send self :default-parameters))
	       (min-mass 0.001))
   (let* ((r (car parameters))
	  (b (cadr parameters))
	  (k (caddr parameters))
	  (mu (car (send self :mean :parameters parameters)))
	  (sigma (car (send self :variance :parameters parameters)))
	  (z (abs (normal-quant min-mass))))
     (apply #'iseq (list (max 0 (- k b) (floor (- mu (* 1.25 z sigma))))
			 (min r k (ceiling (+ mu (* 1.25 z sigma))))))))



;; :mass  -- takes parameters as optional second argument.  First
;; argument should be atoms
(defmeth hypergeometric-family :mass
         (quant &key (parameters (send self :default-parameters)))
   (let* ((r (car parameters))
	  (b (cadr parameters))
	  (k (caddr parameters))
	  (n (+ r b))
	  (normconst (- (+ (log-gamma (1+ k)) (log-gamma (- n k -1)))
			(log-gamma (1+ n)))))
     (exp (+ normconst
	     (- (log-gamma (1+ r))
		(+ (log-gamma (1+ quant)) (log-gamma (- r quant -1))))
	     (- (log-gamma (1+ b))
		(+ (log-gamma (- k quant -1)) (log-gamma (- b (- k quant) -1))))
	     ))))

;; :cdf  -- takes parameters as keyed second argument.  First
;; argument should be quantiles
(defmeth hypergeometric-family :cdf
         (quant &key (parameters (send self :default-parameters))
		     (min-mass nil))
   (unless (listp quant) (setq quant (list quant)))
   (let* ((min-mass (if min-mass min-mass
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


(defmeth hypergeometric-family :mean
         (&key (parameters (send self :default-parameters)))
   (let* ((r (car parameters))
	  (b (cadr parameters))
	  (k (caddr parameters))
	  (n (+ r b)))
     (list (* k (/ r n)))))

(defmeth hypergeometric-family :variance
         (&key (parameters (send self :default-parameters)))
   (let* ((r (car parameters))
	  (b (cadr parameters))
	  (k (caddr parameters))
	  (n (+ r b)))
     (list (* k (/ r n) (/ b n) (/ (- n k) (- n 1))))))




;;; random variable defaults  (again parameters are optional
;;; arguements)

;; :rv-default  --- default value of rv.

(defmeth hypergeometric-family :rv-default
         (&key (parameters (send self :default-parameters)))
   (round (send self :mean :parameters parameters)))


;; :rv-limits
(defmeth hypergeometric-family :rv-limits
         (&key (parameters (send self :default-parameters)))
  (list (list 0 *infty*)))
   
;; :rv-integer?   
(defmeth hypergeometric-family :rv-integer? (&key &allow-other-keys)
  t)

;; :rv-range-default
(defmeth hypergeometric-family :rv-range-default
         (&key (parameters (send self :default-parameters)))
   (let* ((r (car parameters))
	  (b (cadr parameters))
	  (k (caddr parameters))
	  (mu (car (send self :mean :parameters parameters)))
	  (sigma (car (send self :variance :parameters parameters))))
     (apply #'iseq (list (max 0 (- k b) (floor (- mu (* 4 sigma))))
			 (min r k (ceiling (+ mu (* 4 sigma))))))))

		    


;(defmeth hypergeometric-family :continuous? () nil)

(defmeth hypergeometric-family :rand (num &key
			      (parameters (send self :default-parameters)))
  (send self :quantiles (uniform-rand num) :parameters parameters))


(send hypergeometric-family :isnew :name :hypergeometric)
(push hypergeometric-family *Known-families*)


(new-provide :el-hypergeometric)

