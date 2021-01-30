;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the objects for parameter manipulation


(require :el-utils (strcat ElToY-directory "utils" *d-sep*  "utils.lsp"))
(require :el-update (strcat ElToY-directory "utils" *d-sep*  "update.lsp"))
(require :el-checks (strcat ElToY-directory "utils" *d-sep*  "checks.lsp"))
(require :el-constraint (strcat ElToY-directory "utils" *d-sep*  "constraint.lsp"))


;;; *default-granularity* system-wide default
(defvar *default-granularity* 0.1 "Granularity for parameters")
;;; *default-limits* system-wide default
(defvar *default-limits* '(-3000007 3000007) "Hard Limits for Parameters")

;;; para-proto --- mixin for general purpose parameter manipulation.
;;; Meant to be used for parameters, hyperparameters
;;; nui-parameters, or alt-parameters.  Alternatively, the
;;; controlled-parameter-mixin can produce displays.

;;; Note: names must be symbols, not strings

(defproto para-proto
  '(values				;list of parameter values
    names				;coresponding list of
					;parameter names
    range				;range of considered values
					;for parameters
    limits				;hard upper and lower bounds
					;for parameters
    granularity				;granularity for
					;parameter elicitation
    integer?				;is parameter contrained to integers?
    constraint-fun			;constraint-function among parameters
    nparams				;number of parameters
    ) '() (list ElToY-object)
"This is a general parameter handling tool.  
Note: names must be symbols not strings."
)

;; generic methods: :describe :print :isnew

; :print method
(defmeth para-proto :print (&optional (stream t))
   (format stream "#<parameters: (~S) ~S>"
	   (send self :names) 
	   (send self :values) ))


; :describe method --- verbose argument controls amount of excess
; printing done (t or integer) controls printing.
(defmeth para-proto :describe (&optional (stream t) (verbose 1))
   (format stream "The Parameters: ~S~%"
	   (send self :names))
   (format stream "Values are: ~S~%"
	   (send self :values))
   (when (or (eq verbose t)
	     (and (numberp verbose) (>= verbose 1)))
	 (format stream "Range of values ~S~%"
		 (send self :range))
	 (format stream "Integer values? ~S~%"
		 (send self :integer?))
	 (when (or (eq verbose t)
		   (and (numberp verbose) (>= verbose 2)))
	       (format stream "Hard limits of values ~S~%"
		       (send self :limits))
	       (when (or (eq verbose t)
			 (and (numberp verbose) (>= verbose 3)))
		     (format stream "Granularity of values ~S~%"
			     (send self :granularity))))
	 (call-next-method stream verbose)))



; :isnew method
(defmeth para-proto :isnew
	 (&rest args 
	  &key (values nil) (names nil)
	       (range nil) (limits nil)
	       (granularity nil) (integer? nil)
	       (constraint-fun vacuous-constraint)
	       &allow-other-keys)
   ;check for equal list list-lengths
  (unless (listp granularity) (setq granularity (list granularity)))
  (unless (listp integer?) (setq integer? (list integer?)))
  (setf (slot-value 'nparams)
	(max (list-length values) (list-length names)
	     (list-length range) (list-length limits)
	     (list-length granularity) (list-length integer?)))
  ;; integer?
  (if (null integer?) (setq integer? (list nil)))
  (if (eql 1 (list-length integer?))
      (setq integer? (repeat integer? (slot-value 'nparams))))
  (unless (eql (slot-value 'nparams) (list-length integer?))
	  (error "para::isnew --- bad integer?: ~S~%" integer?))
  (unless (or (null values)
	      (and (eql (slot-value 'nparams) (list-length values))
		   (every #'checknump values integer?)))
	  (error "para::isnew --- bad values: ~S" values))
  (unless (or (null names) (eql (slot-value 'nparams) (list-length names)))
	  (error "para::isnew --- bad names: ~S" names))
  (unless (null range)
	  (unless (and (eql (slot-value 'nparams) (list-length range))
		       (every #'test-pair range integer?))
		  (error "para::isnew --- bad range: ~S" range))
	  (unless (or (null values) (every #'between values range))
		  (cerror "Force values within range"
		   "para::isnew --- values not between range: ~S ~S"
			 values range)
		  (setq values (force-between values range))))
  (if (null limits) (setq limits (make-list (slot-value 'nparams)
					  :initial-element *default-limits*))
    (unless (and (eql (slot-value 'nparams) (list-length limits))
		 (every #'test-pair limits integer?))
	    (error "para::isnew --- bad limit: ~S" limits)))
  (unless (or (null values) (every #'between values limits))
	  (cerror "force values between limits"
	   "para::isnew --- values not between limits: ~S ~S"
		  values limits)
	  (setq values (force-between values limits)))
  (unless (or (null range) (every #'between2 range limits))
	  (cerror "force range between limits"
	   "para::isnew --- range not between limits: ~S ~S"
	   range limits)
	  (setq range (force2-between range limits)))
  (if (null range) (setq range limits))
  
  ;; check that constraint-function is satisfied
  (unless (funcall constraint-fun values :warn t)
	  (error "para::isnew --- Unconstrained values: ~S"
		 values))
  
					;granularity is a system-wide default
  (if (null granularity) (setq granularity *default-granularity*))

					;we're ok, 
					;set local parameters
  (send self :names names)
  (setf (slot-value 'integer?) integer?)
  (setf (slot-value 'values) values)
  (setf (slot-value 'range) range)
  (setf (slot-value 'limits) limits)
  (setf (slot-value 'constraint-fun) constraint-fun)
  (send self :granularity granularity)
  (apply #'call-next-method args)
  )

(send para-proto :documentation :isnew
"Para-proto:  :names --- sets names (list of symbols)
              :values --- sets values (list of numbers)
              :range --- sets range (list of number pairs)
              :limits --- sets limits (list of number pairs)
              :granularity --- sets granularity (minimum change values)
              :integer? --- is parameter an integer? (list of (or T nil))
	      :constraint-fun --- constraint function for parameters
")


;; destruct
(defmeth para-proto :destruct (&rest args)
  (setf (slot-value 'values) nil)
  (setf (slot-value 'names) nil)
  (setf (slot-value 'range) nil)
  (setf (slot-value 'limits) nil)
  (setf (slot-value 'granularity) nil)
  (setf (slot-value 'integer?) nil)
  (setf (slot-value 'constraint-fun) nil)
  (setf (slot-value 'nparams) nil)
  (apply #'call-next-method args))


;;; various generic set/send functions

;; values
(defmeth para-proto :values (&optional (values nil set))
   (when set 
	 (unless (and (eql (slot-value 'nparams) (list-length values))
		      (every #'numberp values ))
		 (error "para::values --- bad values: ~S" values))
	 (setq values (round-if values (slot-value 'integer?)))
	 (unless (every #'between values (slot-value 'limits))
		 (uerror "Bring within limits "
			 "para::values --- values outside hard limits: ~S ~S"
			 values (slot-value 'limits)))
	 ;; check that constraint-function is satisfied
	 (unless (funcall (send self :constraint-fun) values :warn t)
		 (error "para::values --- Unconstrained values: ~S"
			values))
 	 (if (every #'between values (slot-value 'range))
	     (setf (slot-value 'values) values)
	   (let* ((newvalrange (mapcar #'force-within values
				       (slot-value 'range)
				       (slot-value 'limits))))
	     (setf (slot-value 'values) (mapcar #'car newvalrange))
	     (send self :range (mapcar #'cdr newvalrange)))))
   (slot-value 'values))

(send para-proto :documentation :values
"Method args: (&optional value-list)
Retrieves/sets list of values (real numbers between limits).

If values not between limits, error signaled.
If values not within range, range is extended.
If value is constrained to be an integer, it is rounded to the nearest
integer. 
")

(defmeth para-proto :values1 (who &optional (values nil set))
   (when set				;setting values
	 (unless (numberp values)
		 (error "para::values1 --- bad value: ~S" values))
	 (if (nth who (slot-value 'integer?))
	     (setq values (round values)))
	 (unless (between values (nth who (slot-value 'limits)))
		 (uerror "Bring within limits "
			 "para::values1 --- value outside hard limits: ~S ~S"
			 values (nth who (slot-value 'limits))))
	 ;; check that constraint-function is satisfied
	 (let ((all-values (copy-list (send self :values))))
	   (setf (nth who all-values) values)
	   (unless (funcall (send self :constraint-fun) all-values :warn t)
		   (error "para::values1 --- Unconstrained values: ~S"
			all-values)))
	   
 	 (if (between values (nth who (slot-value 'range)))
	     (setf (nth who (slot-value 'values)) values)
	   (let* ((newvalrange (force-within values
					     (nth who (slot-value 'range))
					     (nth who (slot-value 'limits)))))
	     (setf (nth who (slot-value 'values)) (car newvalrange))
	     (send self :range1 who (cdr newvalrange)))))
   (nth who (slot-value 'values)))
(send para-proto :documentation :values1
"Method args: (who &optional new-value)
               who is integer
Retrieves/sets nth value in list (real numbers between limits).

If new-value not between limits, error signaled.
If new-value not within range, range is extended.
If new-value is constrained to be an integer, it is rounded.
")


(defmeth para-proto :range (&optional (range nil set))
   (when set 
	 (unless (and (eql (slot-value 'nparams) (list-length range))
		      (every #'test-pair range (slot-value 'integer?)))
		 (error "para::range --- bad range: ~S" range))
	 (unless (every #'between2 range (slot-value 'limits))
		 (uerror "Bring within limits "
			 "para::range --- range outside hard limits: ~S ~S"
			 range (slot-value 'limits)))
	 (setf (slot-value 'range) 
	       (if (every #'between2 range (slot-value 'limits))
		   range
		 (setq range
		       (mapcar #'force2-between range (slot-value 'limits)))))
	 (unless (every #'between (slot-value 'values) range)
		 (send self :values
		       (force-between (slot-value 'values) range))))
   (slot-value 'range))
(send para-proto :documentation :range
"Method args: (&optional range-list)
Retrieves/sets list of ranges (pairs of numbers between limits).

If range not between limits, error is signaled.
If values not within range, values are forced between.
If parameter constrained to be integer, range values must be integers.
")


(defmeth para-proto :range1 (who &optional (range nil set))
  (when set 
	(unless (test-pair range (nth who (slot-value 'integer?)))
		(error "para::range1 --- bad range: ~S" range))
	(unless (between2 range (nth who (slot-value 'limits)))
		(uerror "Bring within limits "
			"para::range1 --- range outside hard limits: ~S ~S"
			range (nth who (slot-value 'limits))))
	(setf (nth who (slot-value 'range)) 
	      (if (between2 range (nth who (slot-value 'limits)))
		  range 
		(setq range
		      (force2-between range (nth who (slot-value 'limits))))))
	(unless (between (nth who (slot-value 'values)) range)
		(send self :values1 (force-between
				     (nth who (slot-value 'values))
				     range))))
  (nth who (slot-value 'range)))

(send para-proto :documentation :range1
"Method args: (who &optional range-list)
               who --- integer
Retrieves/sets  whoth range (pair of numbers between limits).

If range not between limits, error is signaled.
If values not within range, values are forced between.
If parameter constrained to be integer, range values must be integers.
")

;; limits
(defmeth para-proto :limits (&optional (limits nil set))
   (when set 
	 (unless (and (eql (slot-value 'nparams) (list-length limits))
		      (every #'test-pair limits (slot-value 'integer?)))
		 (error "para::limits --- bad limits: ~S" limits))
	 (setf (slot-value 'limits) limits)
 	 (unless (every #'between2 (slot-value 'range) limits)
		 (send self :range 
		       (mapcar #'force2-between (slot-value 'range) limits))))
   (slot-value 'limits))

(send para-proto :documentation :limits
"Method args: (&optional limits-list)
Retrieves/sets list of limits (list of pairs of numbers).

If ranges not between limits, ranges are forced-between.
If values not within limits, values are forced between.
If parameter constrained to be integer, limit values must be integers.
")


(defmeth para-proto :limits1 (who &optional (limits nil set))
   (when set 
	 (unless (test-pair limits (nth who (slot-value 'integer?)))
		 (error "para::limits1 --- bad limits: ~S" limits))
	 (setf (nth who (slot-value 'limits)) limits)
 	 (unless (between2 (nth who (slot-value 'range)) limits)
		 (send self :range1 who
		       (force2-between (nth who (slot-value 'range)) limits))))
   (nth who (slot-value 'limits)))

(send para-proto :documentation :limits1
"Method args: (who &optional limits-list)
               who --- integer
Retrieves/sets  whoth limits (pair of numbers).

If range not between limits, range is forced-between.
If values not within limits, values are forced between.
If parameter constrained to be integer, limit values must be integers.
")


;; :granularity
(defmeth para-proto :granularity (&optional (granularity nil set))
   (when set
	 (if (numberp granularity) 
	       (setq granularity (make-list (slot-value 'nparams)
					    :initial-element granularity))
	   (if (and (listp granularity) (eql (list-length granularity) 1))
	       (setq granularity (make-list (slot-value 'nparams)
					    :initial-element
					    (car granularity)))
	     
	     (unless (and (eql (list-length granularity) (slot-value 'nparams))
			  (every #'numberp granularity))
		     (error "para::granularity --- bad granularity: ~S" granularity))))
	 (setq granularity (ceiling-if granularity (slot-value 'integer?)))
	 (setf (slot-value 'granularity) granularity))
   (slot-value 'granularity))

(send para-proto :documentation :granularity
"Method args: (&optional granularity)
 Retrieves/sets  list granularity.  If value is a single
number or a list with one element, all values are set to same number.
If corresponding parameter is forced to be an integer, takes ceiling
of value.
")


(defmeth para-proto :granularity1 (who &optional (granularity nil set))
   (when set
	 (unless (numberp granularity)
		 (error "para::granularity1 --- bad granularity: ~S"
			granularity))
	 (if (nth who (slot-value 'integer?))
	     (setq granularity (ceiling granularity)))
	 (setf (nth who (slot-value 'granularity)) granularity))
   (nth who (slot-value 'granularity)))

(send para-proto :documentation :granularity1
"Method args: (who &optional granularity)
               who --- integer
Retrieves/sets  whoth granularity.
If parameter is constrained to be an integer, take ceiling of
granularity. 
")

;; integer?
(defmeth para-proto :integer? (&optional (integer? nil set))
   (when set
	 (unless (eql (list-length integer?) (slot-value 'nparams))
		 (error "para:integer? Bad integer? ~S" integer?))
	 (setf (slot-value 'integer?) integer?)
	 (send self :limits (round-if (slot-value 'limits) integer?))
	 (send self :range (round-if (slot-value 'range) integer?))
	 (send self :values (round-if (slot-value 'values) integer?))
	 (send self :granularity
	       (ceiling-if (slot-value 'granularity) integer?)))
   (slot-value 'integer?))

(send para-proto :documentation :integer?
  "Returns/ sets integer constraint.  On set, forces
limits, range, values and granularity to be integers.
")


(defmeth para-proto :integer?1 (who &optional (integer? nil set))
   (when set
	 (setf (nth who (slot-value 'integer?)) integer?)
	 (if integer?
	     (send self :limits1 who
		   (round-if (nth who (slot-value 'limits)) integer?))
	 (send self :range1 who
	       (round-if (nth who (slot-value 'range)) integer?))
	 (send self :values1 who
	       (round-if (nth who (slot-value 'values)) integer?))
	 (send self :granularity1 who
	       (ceiling-if (nth who (slot-value 'granularity)) integer?))))
   (nth who (slot-value 'integer?)))

(send para-proto :documentation :integer?1
  "Returns/ sets integer constraint.  On set, forces
limits, range, values and granularity to be integers.
")

;; :names  changing the names is a little bit different because we
;; must create new methods which go with the appropriate name
(defmeth para-proto :names (&optional (names nil set))
   (when set
	 (unless (eql (list-length names) (slot-value 'nparams))
		 (error "para:names Bad names ~S" names))
	 (mapc #'(lambda (old-name)
		   (send self :delete-method old-name)
		   (send self :delete-documentation old-name))
	       (slot-value 'names))
	 (setf (slot-value 'names) names)
	 (mapc #'(lambda (name)
		   (send self :add-method name
			 #'(lambda (self &rest args)
			     (apply #'para-proto-para-method
				     self (position name names) args)))
		   (send self :documentation name
			 para-proto-para-method-doc))
	       names))
   (slot-value 'names))

(send para-proto :documentation :names
"Method args: (&optional new-name-list)
Retrieves/sets name list

Also defines method for parameter object for each name <new-name> which
returns/sets parameter values (default),range,limits, and granularity.
")




(defmeth para-proto :names1 (who &optional (new-name nil set))
  (when set
	  (send self :delete-method (nth who (slot-value 'names)))
	  (send self :delete-documentation (nth who (slot-value 'names)))
	  (setf (nth who (slot-value 'names)) new-name)
	  (send self :add-method new-name
		#'(lambda (self &rest args)
		    (apply #'para-proto-para-method
			    self (position name names) args)))
	  (send self :documentation new-name para-proto-para-method-doc))
  (nth who (slot-value 'names)))

(send para-proto :documentation :names1
"Method args: (who &optional new-name)
               who --- integer
Retrieves/sets  whoth name

Also defines method for parameter object with name <new-name> which
returns/sets parameter values (default),range,limits, and granularity.
")



;;; generic name handler.  For each parameter name, the para-proto
;;; assigns a method.  The method takes several signals:  

(defvar para-proto-para-method-doc 
" With no argument, this method retuns the named parameter value
  With a numeric arugment, this method sets the named parameter value
  With signal arguments:
     :value or :values (or omited/numeric value) returns/sets the value
     :range --- returns/changes the range
     :limit, :limits --- returns/changes the limits
     :granularity --- returns/changes the granularity
     :integer? --- returns/changes the force intger flag
     :name, :names --- returns/changes the name
")

(defun para-proto-para-method (self who &rest args &aux (signal :value))
  (unless (or (null args) (numberp (car args)))
	  (setq signal (pop args)))
  (case signal
     ((:value :values) (apply #'send  self :values1 who args))
     (:range (apply #'send  self :range1 who args))
     ((:limit :limits) (apply #'send  self :limits1 who args))
     (:granularity (apply #'send  self :granularity1 who args))
     ((:integer :integer?) (apply #'send  self :integer?1 who args))
     ((:name :names) (apply #'send  self :names1 who args))
     (t (error "para::bad parameter message: ~S" args))))

;
;;; Constraint-functions

(defmeth para-proto :constraint-fun (&optional (new-fun nil set))
   (when set
	 (unless (functionp new-fun)
		 (error "para::constraint-fun Bad function ~S" new-fun))
	 (setf (slot-value 'constraint-fun) new-fun))
   (slot-value 'constraint-fun))

(send para-proto :documentation :constraint-fun
      "Returns/Sets additional constraint function for parameters.

Constraint functions should take a list of parameters and an optional
:warn keyword arg (default t).  They may take additional keyword args.
They should return t if the constraints among the parameters are
satisfied and nil otherwise.  If :warn is t, they should print a
warning message describing the error.")



(new-provide :el-parameters)

