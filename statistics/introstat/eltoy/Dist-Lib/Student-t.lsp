;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines student-t distribution families.

(require :el-family (strcat ElToY-directory "fam" *d-sep*  "family.lsp"))

;;; generic student-t family  Note that a family is a proto-type which is
;;; sent a :new message to create a specific instance of that family

(defproto student-t-family '() '() (list continuous-family-proto))

;;; :isnew --- mostly this sets up default args for the call to the
;;; family-proto :isnew
(defmeth student-t-family :isnew
  	(&rest args
	 &key (name (gensym "STUDENT-T"))
	      (rv-names '(X))
	      (parameter-names '(d.f. Location Scale))
	      (default-parameters '(4 0 1))
	      (parameter-range-default '((1 30) (-10 10) (0 5)))
	      (parameter-limits (list (list 0 *infty*)
				      (list *-infty* *infty*)
				      (list 0 *infty*)))
	      (parameter-granularity (list 1 .1 .1))
	      (parameter-integer? (list t nil nil))
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
;;; specific to student-t are maintained.

;; sensible default for print
(send student-t-family :name :student-t)

;;; required distribution function methods

;; :quantiles  -- takes parameters as optional second argument.  First
;; argument should be probabilities expressed as fraction
(defmeth student-t-family :quantiles
         (prob &key (parameters (send self :default-parameters)))
   (let ((df (first parameters))
	 (location (second parameters))
	 (scale (third parameters)))
     (+ location (* scale (t-quant prob df)))))


;; :density  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth student-t-family :density
         (quant &key (parameters (send self :default-parameters)))
   (let ((df (first parameters))
	 (location (second parameters))
	 (scale (third parameters)))
     (/ (t-dens (/ (- quant location) scale) df)
	scale)))

;; :cdf  -- takes parameters as optional second argument.  First
;; argument should be quantiles
(defmeth student-t-family :cdf
         (quant &key (parameters (send self :default-parameters)))
   (let ((df (first parameters))
	 (location (second parameters))
	 (scale (third parameters)))
     (student-t-cdf (/ (- quant location) scale)) df))
	

;;; random variable defaults  (again parameters are optional
;;; arguements)

;; :rv-default  --- default value of rv.

(defmeth student-t-family :rv-default
         (&key (parameters (send self :default-parameters)))
   (list (second parameters)))		;location (median)



;; :rv-limits
(defmeth student-t-family :rv-limits
         (&key (parameters (send self :default-parameters)))
   (list (list *-infty* *infty*)))


;; :rv-range-default
(defmeth student-t-family :rv-range-default
         (&key (parameters (send self :default-parameters)))
   (let ((df (first parameters))
	 (location (second parameters))
	 (scale (third parameters)))
     (list (+ (* '(-1 1) (+ 3 (/1 df)) scale) location))))


(defmeth student-t-family :rand (num &key
			      (parameters (send self :default-parameters)))
  (let ((df (first parameters))
	 (location (second parameters))
	 (scale (third parameters)))
    (+ (* (t-rand num df) scale) location)))

(defmeth student-t-family :mean (&key
			      (parameters (send self :default-parameters)))
  (let ((df (first parameters))
	 (location (second parameters))
	 (scale (third parameters)))
    (if (> df 1)
	(list location)
      (progn (warn "Student-t with 1 df does not have mean~%") nil))))

(defmeth student-t-family :variance (&key
			      (parameters (send self :default-parameters)))
  (let ((df (first parameters))
	 (location (second parameters))
	 (scale (third parameters)))
    (if (> df 2)
	(list (* scale scale (/ df (- df 2))))
      (progn (warn "Student-t with <3 df does not have variance~%") nil))))

      

;(defmeth student-t-family :continuous? () t)

(send student-t-family :isnew :name :student-t)
(push student-t-family *Known-families*)

(new-provide :el-student-t)



