;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines cont-uniform distribution families.

(require :el-family (strcat ElToY-directory "fam" *d-sep*  "family.lsp"))

;;; generic cont-uniform family  Note that a family is a proto-type which is
;;; sent a :new message to create a specific instance of that family

(defproto cont-uniform-family '() '() (list continuous-family-proto))

;;; :isnew --- mostly this sets up default args for the call to the
;;; family-proto :isnew
(defmeth cont-uniform-family :isnew
  	(&rest args
	 &key (name (gensym "CONT-UNIFORM"))
	      (rv-names '(X))
	      (parameter-names '(min max))
	      (default-parameters '(0 1))
	      (parameter-range-default '((-10 10) (0  20)))
	      (parameter-limits (list (list (- *infty*) *infty*)
				      (list (- *infty*) *infty*)))
	      (parameter-granularity (list 0.5 0.5))
	      (parameter-integer? `(nil nil))
	      (parameter-constraint-fun #'increasing-constr-fun)
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


;;; most methods are inherited from family-proto  only those functions
;;; specific to cont-uniform are maintained.

;; sensible default for print
(send cont-uniform-family :name :cont-uniform)

;;; required distribution function methods

;; :quantiles  -- takes parameters as optional second argument.  First
;; argument should be probabilities expressed as fraction
(defmeth cont-uniform-family :quantiles
         (prob &key (parameters (send self :default-parameters)))
 (let ((min (car parameters))
       (max (cadr parameters)))
   (+ (* prob (- max min)) min)))


;; :density  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth cont-uniform-family :density
         (quant &key (parameters (send self :default-parameters)))
  (unless (listp quant) (setq quant (list quant)))
  (let* ((min (car parameters))
	 (max (cadr parameters))
	 (c (/ 1 (- max min))))
    (mapcar #'(lambda (q)  (if (and (< q max) (> q min)) c 0))
	    quant)))

;; :cdf  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth cont-uniform-family :cdf
         (quant &key (parameters (send self :default-parameters)))
  (unless (listp quant) (setq quant (list quant)))
  (let* ((min (car parameters))
	 (max (cadr parameters))
	 (c (/ 1 (- max min))))
    (mapcar #'(lambda (q)
		(cond ((< q min) 0)
		      ((> q max) 1)
		      (t (* c (- q min)))))
	    quant)))


;;; random variable defaults  (again parameters are optional
;;; arguements)

;; :rv-default  --- default value of rv.

(defmeth cont-uniform-family :rv-default
         (&key (parameters (send self :default-parameters)))
   (send self :quantiles '(.5) parameters))


;; :rv-limits
(defmeth cont-uniform-family :rv-limits
         (&key (parameters (send self :default-parameters)))
   parameters)



;; :rv-range-default
(defmeth cont-uniform-family :rv-range-default
         (&key (parameters (send self :default-parameters)))
  parameters)


(defmeth cont-uniform-family :rand (num &key
				(parameters (send self :default-parameters)))
  (let ((min (car parameters))
	(max (cadr parameters)))
    (+ (* (uniform-rand num) (- max min)) min)))

(defmeth cont-uniform-family :mean (&key
				(parameters (send self :default-parameters)))
  (let ((min (car parameters))
	(max (cadr parameters)))
    (list (/ (+ max min) 2))))

(defmeth cont-uniform-family :variance (&key
				(parameters (send self :default-parameters)))
  (let ((min (car parameters))
	(max (cadr parameters)))
    (list (/ (** (- max min) 2) 12))))


;(defun uniform-rand (num)
;  (/ (random (repeat *infty* num)) *infty))


;(defmeth cont-uniform-family :continuous? () t)

(send cont-uniform-family :isnew :name :cont-uniform)
(push cont-uniform-family *Known-families*)

(new-provide :el-uniform)
