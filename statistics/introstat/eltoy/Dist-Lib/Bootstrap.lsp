;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines binomial distribution families.

(require :el-family (strcat ElToY-directory "fam" *d-sep*  "family.lsp"))

;;; generic Bootstrap distributions.
;;; finite-family is a finite distribution specified by its atoms and
;;; mass. 
;;; Bootstrap-family is a bootstrap with mass 1/n for each data point.


(defproto finite-family '(atoms mass) '() (list discrete-family-proto)
  "Arbitrary finite discrete distribution specified by atoms and mass
")

;;; :isnew --- mostly this sets up default args for the call to the
;;; family-proto :isnew
(defmeth finite-family :isnew
  	(&rest args
	 &key (name (gensym "FINITE"))
	      (atoms '())
	      (mass '())
	      (rv-names '(X))
	      (parameter-names '())
	      (default-parameters '())
	      (parameter-range-default '())
	      (parameter-limits '())
	      (parameter-granularity '())
	      (parameter-integer? '())
	      &allow-other-keys)
  (let* ((order (order atoms))
	 (o-atoms (remove-duplicates (select atoms order)))
	 (o-mass (mapcar #'(lambda (atom)
			     (sum (select mass (which (= atoms atom)))))
			 o-atoms)))
    (setf (slot-value 'atoms) o-atoms)
    (setf (slot-value 'mass) o-mass)
    (apply #'call-next-method
	   :name name
	   :rv-names rv-names
	   :parameter-names parameter-names
	   :default-parameters default-parameters
	   :parameter-range-default parameter-range-default
	   :parameter-limits parameter-limits
	   :parameter-integer? parameter-integer?
	   :parameter-granularity parameter-granularity
	   args)))



;;; most methods are inherited from family-proto  only those functions
;;; specific to finite are maintained.

;; sensible default for print
(send finite-family :name :finite)

(defmeth finite-family :print (&optional (stream t))
  (format stream "#<~A-distribution over ~S>"
	  (send self :name)
	  (slot-value 'atoms)))


;;; required distribution function methods

;; :quantiles
(defmeth finite-family :quantiles (prob &key (parameters nil))
  (unless (listp prob) (setq prob (list prob)))
  (let* ((cdf (cumsum (slot-value 'mass)))
	 (pos (mapcar #'(lambda (pr)
			  (position pr cdf :test #'<=))
		      prob)))
    
    (select (slot-value 'atoms) pos)))

    


;; Bootstrap.lsp  -- 
(defmeth finite-family :atoms (&key (parameters nil)
				    (min-mass nil)
				    (set nil))
  (when set
      (setf (slot-value 'atoms) (remove-duplicates (sort-data set)))
      (setf (slove-value 'mass) (rep (/1 (length set)) (length set))))
  (slot-value 'atoms))



;; :mass  -- takes parameters as optional second argument.  First
;; argument should be atoms
(defmeth finite-family :mass (quant &key
				    (parameters (send self :default-parameters))
				    set)
  (unless (listp quant) (setq quant (list quant)))
  (let ((pos (mapcar #'(lambda (q) (position q (slot-value 'atoms)))
		     quant)))
    (if set
	(mapcar #'(lambda (m p)
		    (if p (setf (nth p (slot-value 'mass)) m)))
		set pos)
      (mapcar #'(lambda (p)
		  (if p (nth p (slot-value 'mass))
		    0))
	      pos))))



;; :cdf  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth finite-family :cdf
         (quant &key (parameters (send self :default-parameters)))
  (unless (listp quant) (setq quant (list quant)))
  (let* ((K (length (slot-value 'atoms)))
	 (pos (mapcar #'(lambda (q)
			  (let ((p (position q (slot-value 'atoms) :test #'<)))
			    (if p p K)))
		      quant))
	 (cdf (cumsum (cons 0 (slot-value 'mass)))))
    (select cdf pos)))



;;; random variable defaults  (again parameters are optional
;;; arguements)

;; :rv-default  --- default value of rv.

(defmeth finite-family :rv-default
         (&key (parameters (send self :default-parameters)))
   (send self :quantiles `(.5)))

;; :rv-limits
(defmeth finite-family :rv-limits
         (&key (parameters (send self :default-parameters)))
   (let ((atoms (slot-value 'atoms)))
     (cons (first atoms) (last atoms))))
	 
;; :rv-integer?   
(defmeth finite-family :rv-integer? (&key &allow-other-keys)
  nil)
   

;; :rv-range-default
(defmeth finite-family :rv-range-default
         (&key (parameters (send self :default-parameters)))
  (let ((atoms (slot-value 'atoms)))
    (cons (first atoms) (last atoms))))



;(defmeth finite-family :continuous? () nil)

(defmeth finite-family :rand (num &key
			      (parameters (send self :default-parameters)))
  (send self :quantiles (uniform-rand num)))

(defmeth finite-family :mean (&key
			      (parameters (send self :default-parameters)))
  (list (sum (* (slot-value 'atoms) (slot-value 'mass)))))

(defmeth finite-family :variance (&key
			      (parameters (send self :default-parameters)))
  (list (- (sum (* (slot-value 'atoms) (slot-value 'atoms) (slot-value 'mass)))
	   (** (sum (* (slot-value 'atoms) (slot-value 'mass))) 2))))


    

;(send finite-family :isnew :name :finite)
;(push finite-family *Known-families*)


;
;;; -------- Bootstrap family --------------------

(defproto bootstrap-family '(data) '() (list finite-family)
  "Arbitrary finite discrete distribution specified by atoms and mass
")

;;; :isnew --- mostly this sets up default args for the call to the
;;; family-proto :isnew
(defmeth bootstrap-family :isnew
  	(&rest args
	 &key (name (gensym "BOOTSTRAP"))
	      (data '())
	      (rv-names '(X))
	      (parameter-names '())
	      (default-parameters '())
	      (parameter-range-default '())
	      (parameter-limits '())
	      (parameter-granularity '())
	      (parameter-integer? '())
	      &allow-other-keys)
  (let* ((order (order data))
	 (atoms (remove-duplicates (select data order)))
	 (K (length data))
	 (mass (mapcar #'(lambda (atom)
			   (/ (count atom data) K))
		       atoms)))
    (setf (slot-value 'data) data)
    (apply #'call-next-method
	   :atoms atoms
	   :mass mass
	   :name name
	   :rv-names rv-names
	   :parameter-names parameter-names
	   :default-parameters default-parameters
	   :parameter-range-default parameter-range-default
	   :parameter-limits parameter-limits
	   :parameter-integer? parameter-integer?
	   :parameter-granularity parameter-granularity
	   args)))
  


;;; most methods are inherited from family-proto  only those functions
;;; specific to bootstrap are maintained.

;; sensible default for print
(send bootstrap-family :name :bootstrap)

(defmeth bootstrap-family :print (&optional (stream t))
  (format stream "#<bootstrap-distribution over ~S>"
	  (slot-value 'data)))


(defmeth bootstrap-family :data (&optional set)
  (if set
      (prog1 (setf (slot-value 'data) data)
	(let * ((order (order data))
		(atoms (remove-duplicates (select data order)))
		(K (length data))
		(mass (mapcar #'(lambda (atom)
				  (/ (count atom data) K))
			      atoms)))
	     (setf (slot-value 'atoms) atoms)
	     (setf (slot-value 'mass) mass)))
    (slot-value 'data)))


(defmeth bootstrap-family :rand (num &key
				     (parameters (send self
						       :default-parameters)))
  (sample (slot-value 'data) num t	;with replacement
	  ))

(defmeth bootstrap-family :mean (&key
				     (parameters (send self
						       :default-parameters)))
  (list (mean (slot-value 'data))))

(defmeth bootstrap-family :variance (&key
				     (parameters (send self
						       :default-parameters)))
  (let ((sigma (standard-deviation (slot-value 'data)))
	(n (count-elements (slot-value 'data))))
    (list (/ (* sigma sigma (- n 1)) n))))




(new-provide :el-bootstrap)

