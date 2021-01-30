;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy and use this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the distribution family prototypes for an elicitation tool

(require :el-utils (strcat ElToY-directory "utils" *d-sep*  "utils.lsp"))
(require :el-update (strcat ElToY-directory "utils" *d-sep*  "update.lsp"))
(require :el-constraint (strcat ElToY-directory "utils" *d-sep*  "constraint.lsp"))
(require :el-checks (strcat ElToY-directory "utils" *d-sep*  "checks.lsp"))

;;; family-proto --- Prototype of a family.
(defproto family-proto
  '(
    rv-names		       ;random variable name
    mrv			       ;number (dimensonality) of r.v.
    parameter-names	       ;distribuiton parameter names
    default-parameters			;default values for parameters
    parameter-limits			;hard limits for parameter
					;values
    parameter-range-default		;default range for parameters
    parameter-granularity		;granularity of parameter
					;sliders
    parameter-integer?			;constrain parameter to integer?
    parameter-constraint-fun		;further constraints among parameters
    nparams				;number of parameters
    ) '() (list named-object)
      "The family-proto prototype is really a
meta-class.  A family is itself a prototype which is used to create
specific family objects for specific tools.

See normal-family for description of use.
"
)

;;; select/set messages

(defmeth family-proto :rv-names (&optional (rv-names nil set))
   (when set
	 (unless (eql (list-length rv-names) (slot-value 'mrv))
		 (error "family:names Bad names ~S" rv-names))
	 (setf (slot-value 'rv-names) rv-names))
   (slot-value 'rv-names))

(send family-proto :documentation :rv-names
      "Returns/Sets random variable names")

(defmeth family-proto :parameter-names (&optional (parameter-names nil set))
   (when set
	 (unless (eql (list-length parameter-names) (slot-value 'nparams))
		 (error "family:names Bad names ~S" parameter-names))
	 (setf (slot-value 'parameter-names) parameter-names))
   (slot-value 'parameter-names))

(send family-proto :documentation :parameter-names
      "Returns/Sets parameter names")


(defmeth family-proto :parameter-constraint-fun (&optional (new-fun nil set))
   (when set
	 (unless (functionp new-fun)
		 (error "family::parameter-constraint-fun Bad function ~S"
			new-fun)) 
	 (setf (slot-value 'parameter-constraint-fun) new-fun))
   (slot-value 'parameter-constraint-fun))

(send family-proto :documentation :parameter-constraint-fun
      "Returns/Sets additional constraint function for parameters.

Constraint functions should take a list of parameters and an optional
:warn keyword arg (default t).  They may take additional keyword args.
They should return t if the constraints among the parameters are
satisfied and nil otherwise.  If :warn is t, they should print a
warning message describing the error.
")


(defmeth family-proto :default-parameters
  	(&optional (default-parameters nil set))
   (when set 
	 (unless (and (eql (slot-value 'nparams)
			   (list-length default-parameters))
		      (every #'numberp default-parameters))
		 (error "family::default-parameters --- bad values: ~S"
			default-parameters))
	 (setq default-parameters
	       (round-if default-parameters (slot-value :parameter-integer?)))
	 (unless (every #'between default-parameters
			          (slot-value 'parameter-limits))
		 (uerror "Bring within limits "
			 "family::default-parameters --- values outside hard limits: ~S ~S"
			 default-parameters (slot-value 'parameter-limits)))
	 (unless (funcall (send self :parameter-constraint-fun)
			default-parameters :warn t)
		 (error "family::default-parameters --- Unconstrained values: ~S"
			default-parameters))
 	 (if (every #'between default-parameters
		              (slot-value 'parameter-range-default))
	     (setf (slot-value 'default-parameters) default-parameters)
	   (let* ((newvalrange
		   (force-within default-parameters
				 (slot-value 'parameter-range-default)
				 (slot-value 'parameter-limits))))
	     (setf (slot-value 'default-parameters) (mapcar #'car newvalrange))
	     (send self :parameter-range-default (mapcar #'cdr newvalrange)))))
   (slot-value 'default-parameters))

(send family-proto :documentation :default-parameters
      "Returns/Sets default parameter values
Checks for consistency among values, range, limits and integer constraints.
")



(defmeth family-proto :parameter-limits (&optional (parameter-limits nil set))
   (when set 
	 (unless (and (eql (slot-value 'nparams)
			   (list-length parameter-limits))
		      (every #'test-pair parameter-limits))
		 (error "isnew::parameter-limits --- bad limits: ~S"
			parameter-limits))
	 (setq parameter-limits
	       (round-if parameter-limits (slot-value 'parameter-integer?)))
	 (setf (slot-value 'parameter-limits) parameter-limits)
 	 (unless (every #'between2 (slot-value 'parameter-range-default)
			           parameter-limits)
		 (send self :parameter-range-default 
		       (mapcar #'force2-between
			       (slot-value 'parameter-range-default)
			       parameter-limits))))
   (slot-value 'parameter-limits))

(send family-proto :documentation :parameter-limits
      "Returns/Sets parameter limits
Checks consistency among values, range, limits and integer constraints.
")


(defmeth family-proto :parameter-range-default
         (&optional (parameter-range-default nil set))
   (when set 
	 (unless (and (eql (slot-value 'nparams)
			   (list-length parameter-range-default))
		      (every #'test-pair parameter-range-default))
		 (error "family::range --- bad range: ~S"
			parameter-range-default))
	 (setq parameter-range-default
	       (round-if parameter-range-default
			 (slot-value 'parameter-integer?)))
	 (unless (every #'between2 parameter-range-default
			(slot-value 'parameter-limits))
		 (uerror "Bring within limits "
			 "family::range --- parameter range outside hard limits: ~S ~S"
			 parameter-range-default
			 (slot-value 'parameter-limits)))
 	 (if (every #'between2 parameter-range-default
		    (slot-value 'parameter-limits))
	     (setf (slot-value 'parameter-range-default)
		   parameter-range-default)
	   (setq parameter-range-default
		 (setf (slot-value 'parameter-range-default)
		       (mapcar #'force2-between parameter-range-default
			       (slot-value 'parameter-limits)))))
	 (unless (every #'between (slot-value 'default-parameters)
			parameter-range-default)
		 (send self :default-parameters
		       (force-between (slot-value 'default-parameters)
				      parameter-range-default))))
   (slot-value 'parameter-range-default))

(send family-proto :documentation :parameter-range-default
      "Returns/Sets default parameter range
Checks consistency among values, range, limits and integer constraints.
")


(defmeth family-proto :parameter-granularity
  (&optional (parameter-granularity nil set))
  (when set
	(if (zerop (slot-value 'nparams))
	    (setq parameter-granularity nil)
	  (progn 
	    (unless (listp parameter-granularity)
		    (setq parameter-granularity (list parameter-granularity)))
	    (if (eql (list-length parameter-granularity) 1)
		(setq parameter-granularity
		      (make-list (slot-value 'nparams)
				 :initial-element
				 (car parameter-granularity))))
	    (setq parameter-granularity
		  (ceiling-if parameter-granularity
			      (slot-value 'parameter-integer?)))
	    (unless (and (eql (list-length parameter-granularity)
			      (slot-value 'nparams))
			 (every #'numberp parameter-granularity))
		    (error "family::granularity --- bad granularity: ~S"
			   parameter-granularity))))
	(setf (slot-value 'parameter-granularity) parameter-granularity))
  (slot-value 'parameter-granularity))


(send family-proto :documentation :parameter-granularity
      "Returns/Sets default parameter granularity 
Controls size of steps on sliders.
")



(defmeth family-proto :parameter-integer? (&optional (integer? nil set))
   (when set
	 (unless (eql (list-length integer?) (slot-value 'nparams))
		 (error "para:integer? Bad integer? ~S" integer?))
	 (setf (slot-value 'parameter-integer?) integer?)
	 (send self :parameter-limits
	       (round-if (slot-value 'parameter-limits) integer?))
	 (send self :parameter-range-default
	       (round-if (slot-value 'parameter-range-default) integer?))
	 (send self :default-parameters
	       (round-if (slot-value 'default-parameters) integer?))
	 (send self :parameter-granularity
	       (ceiling-if (slot-value 'parameter-granularity) integer?)))
   (slot-value 'parameter-integer?))

(send family-proto :documentation :parameter-integer?
  "Returns/ sets integer constraint.  On set, forces
limits, range, values and granularity to be integers.
")


;;; :isnew initialization method

(defmeth family-proto :isnew
	 (&rest args 
	  &key (rv-names '(X)) (parameter-names '(theta))
	       (default-parameters '(0)) 
	       (parameter-range-default (list *default-limits*) )
	       (parameter-limits (list *default-limits*))
	       (parameter-granularity (list *default-granularity*))
	       (parameter-integer? (list nil))
	       (parameter-constraint-fun #'vacuous-constraint)
	       (name nil)
	       &allow-other-keys)
	 ; inherited method
  (send self :name name)

	 ; start with rv-names
  (setf (slot-value 'mrv) (list-length rv-names))
  (send self :rv-names rv-names)

  ; now do parameters
  (unless (listp parameter-granularity)
	  (setq parameter-granularity (list parameter-granularity)))
  (unless (listp parameter-integer?)
	  (setq parameter-integer? (list parameter-integer?)))

  (setf (slot-value 'nparams)
	 (max (list-length default-parameters) (list-length parameter-names)
	      (list-length parameter-range-default)
	      (list-length parameter-limits)
	      (list-length parameter-granularity)
	      (list-length parameter-integer?)))
  (unless (zerop (slot-value 'nparams))
	  (if (null parameter-integer?) (setq parameter-integer? (list nil)))
	  (if (eql 1 (list-length parameter-integer?))
	      (setq parameter-integer?
		    (repeat parameter-integer? (slot-value 'nparams)))) 
	  
	  (unless (and (eql (slot-value 'nparams)
			    (list-length default-parameters))
		       (every #'checknump default-parameters
			      parameter-integer?))
		  (error "family::isnew --- bad default values: ~S~%"
			 default-parameters))

	  (unless (eql (slot-value 'nparams) (list-length parameter-names))
		  (error "family::isnew --- bad names: ~S" parameter-names))

	  (unless (null parameter-range-default)
		  (unless (and (eql (slot-value 'nparams)
				    (list-length parameter-range-default))
			       (every #'test-pair parameter-range-default
				      parameter-integer?))
			  (error "familty::isnew --- bad range-default: ~S"
				 parameter-range-default))
		  (unless (every #'between default-parameters
				 parameter-range-default)
			  (error "family::isnew --- default-parameters not ~
				      between range: ~S ~S"
				 default-parameters parameter-range-default)))
  
	  (if (null parameter-limits)
	      (setq parameter-limits (make-list (slot-value 'nparams)
						:initial-element
						*default-parameter-limits*))
	    (unless (and (eql (slot-value 'nparams)
			      (list-length parameter-limits))
			 (every #'test-pair parameter-limits
				parameter-integer?))
		    (error "family::isnew --- bad limits: ~S"
			   parameter-limits)))
	  (unless (every #'between default-parameters parameter-limits)
		  (error "family::isnew --- default-parameters not ~
			     between limits: ~S ~S"
			 default-parameters parameter-limits))

	  (unless (or (null parameter-range-default)
		      (every #'between2 parameter-range-default
			     parameter-limits))
		  (error "family::isnew --- range not between ~
			   parameter-limits: ~S ~S"
			 parameter-range-default parameter-limits))
	  (if (null parameter-range-default)
	      (setq parameter-range-default parameter-limits))

	  ;test constraint function satisfied.

	  (unless (funcall parameter-constraint-fun default-parameters :warn t)
		  (error "family::isnew --- Unconstrained values: ~S"
			 default-parameters))
	  
	  ;;granularity is a system-wide default
	  (if (null parameter-granularity)
	      (setq parameter-granularity (list *default-granularity*)))
	  )
   ;we're ok, 
   ;set local parameters
   (send self :parameter-names parameter-names)
   (setf (slot-value 'parameter-integer?) parameter-integer?)
   (setf (slot-value 'default-parameters) default-parameters)
   (setf (slot-value 'parameter-range-default) parameter-range-default)
   (setf (slot-value 'parameter-limits) parameter-limits)
   (setf (slot-value 'parameter-constraint-fun) parameter-constraint-fun)
   (send self :parameter-granularity parameter-granularity)
   self
   )

(send family-proto :documentation :isnew
  (strcat
   (send named-object :documentation :isnew)
   "
:rv-names :parameter-names (rv and parameter namelists)
:parameter-limits (hard parameter limits)
:parameter-integer? (integer constraint flags)
:default-parameters :paramter-range-defaults :parameter-granularity
      (defaults for elicitation)
"))





;;; :describe :print method
; :print method
(defmeth family-proto :print (&optional (stream t))
   (format stream "#<~A-family: ~S (~S)>"
	   (send self :name)
	   (send self :rv-names) 
	   (send self :parameter-names)))

;; this makes sure things don't print invisibly.
(send family-proto :name :proto)

; :describe method --- verbose argument controls amount of excess
; printing done (t or integer).
(defmeth family-proto :describe (&optional (stream t) (verbose 1))
   (format stream "The R.V.'s: ~S~%"
	   (send self :rv-names))
   (format stream "The Parameters are: ~S~%"
	   (send self :parameter-names))
   (when (or (eq verbose t)
	     (and (numberp verbose) (>= verbose 1)))
	 (format stream "Parameter default values ~S~%"
		       (send self :default-parameters))
	 (format stream "Hard limits of parameter-values~S~%"
		       (send self :parameter-limits))
	 (format stream "Integer constraints on parameter-values~S~%"
		       (send self :parameter-integer?))
	 (when (or (eq verbose t)
		   (and (numberp verbose) (>= verbose 2)))
	       (format stream "Default range of parameter values ~S~%"
		       (send self :parameter-range-default))
	       (when (or (eq verbose t)
			 (and (numberp verbose) (>= verbose 3)))
		     (format stream "Granularity of parameter values~S~%"
			     (send self :parameter-granularity)))))
   (call-next-method stream))


;;; :destruct

(defmeth family-proto :destruct (&rest args)
  (setf (slot-value 'rv-names) nil)
  (setf (slot-value 'mrv) nil)
  (setf (slot-value 'parameter-names) nil)
  (setf (slot-value 'default-parameters) nil)
  (setf (slot-value 'parameter-limits) nil)
  (setf (slot-value 'parameter-range-default) nil)
  (setf (slot-value 'parameter-granularity) nil)
  (setf (slot-value 'parameter-integer?) nil)
  (setf (slot-value 'parameter-constraint-fun) nil)
  (setf (slot-value 'nparams) nil)
  (apply #'call-next-method args))

;
;;; Specialized Families


;;; --- Discrete-family-proto ---

(defproto discrete-family-proto '() '() (list family-proto)
  "Discrete probabilty distribution specialization")

(send discrete-family-proto :name :|Discrete|)

;;; --- continuous-family-proto ---

(defproto continuous-family-proto '() '() (list family-proto)
  "Discrete probabilty distribution specialization")

;; to get things to print right.
(send continuous-family-proto :name :|Continuous|)


;;; ---- Undefined-family ----

;;; Undefined-family --- this is the default for several uses.
(defproto Undefined-family '() '()
  (list family-proto))


(defmeth Undefined-family :isnew (&rest args
				  &key (name (gensym "Undefined-family"))
				  &allow-other-keys)
  (apply #'call-next-method :name name args))


;; to get things to print right.
(send Undefined-family :name :|Undefined|)

;
;;; :distribution maniputlation function

;;; :quantiles --- must be defined for each family
(defmeth family-proto :quantiles
  	(probabilities &key (parameters nil))
   (error "~A implementation does not have required :quantiles method"
	  self))

(send family-proto :documentation :quantiles
      "probabilities &key (parameters)
Returns quantiles of distribution corresponding to <probabilities>
evaluated at <parameters>.
")


;;; :cdf
(defmeth family-proto :cdf
  	(quantiles &key (parameters nil))
   (error "~A implementation does not have required :cdf method"
	  self))


(send family-proto :documentation :cdf
      "quantiles &key (parameters)
Returns cumulative distribution function corresponding to <quantiles>
evaluated at <parameters>.
")


;;; :rv-default --- must be defined for each family
(defmeth family-proto :rv-default
  	(&rest args &key parameters)
  (apply #'send self :quantiles '(.5) args))

(send family-proto :documentation :rv-default
      "&key (parameters)
Returns default (initial) values for random variable.
")


;;; :rv-limits
(defmeth family-proto :rv-limits
  (&rest args
   &key (parameters nil)
   &allow-other-keys)
  (error "~A implementation does not have required :rv-limits method"
	 self))

(send family-proto :documentation :rv-limits
      "&key (parameters)
Returns hard limits for random variable(s).
")


;;; :data-range
(defmeth family-proto :rv-range-default
  (&rest args)
  (apply #'send self :rv-limits args))


(send family-proto :documentation :rv-range
      "&key (parameters)
Returns elicitation range for random variable(s).
")

;;; :rv-integer?
(defmeth family-proto :rv-integer?
  (&rest args
   &key (parameters nil)
   &allow-other-keys)
  (error "~A implementation does not have required :rv-integer? method"
	 self))

(send family-proto :documentation :rv-integer?
      "&key (parameters)
Are values of r.v. restricted to integers?
")


;;; :continuous?
(defmeth family-proto :continuous?
  (&rest args
   &key (parameters nil)
   &allow-other-keys)
  (error "~A implementation does not have required :continuous? method"
	 self))

(send family-proto :documentation :continuous?
      "
Returns t if distribution is continuous/nil otherwise.
")

;;; :dist-graph-proto --- set in [graph.lsp]
(defmeth family-proto :dist-graph-proto (&key
				 &allow-other-keys)
  (error "~A implementation does not have required :dist-graph-proto
method" self))

(send family-proto :documentation :dist-graph-proto
      "Prototype of graph for displaying this type of distribution.")



;;; :rand
(defmeth family-proto :rand (num &key
				 (parameters (send self :default-parameters))
				 &allow-other-keys)
  (send self :quantiles (uniform-rand num) :parameters parameters))

(send family-proto :documentation :rand
      "Args: num &key parameters
Random number generator, generates num numbers.
")


(defmeth family-proto :mean
  	(&rest args &key parameters)
  (error "~A implementation does not have required :mean method"
	 self))

(send family-proto :documentation :mean
      "&key (parameters)
Returns list of means (expectation) for random variables.
")

(defmeth family-proto :variance
  	(&rest args &key parameters)
  (error "~A implementation does not have required :variance method"
	 self))

(send family-proto :documentation :variance
      "&key (parameters)
Returns list of variances (2nd moment about mean) for random variables.
")

;
;;;------------------------- Continuous Distributions ------------------
;; these functions are required for the proper behavior of continuous
;; distributions. 

;;; :continuous?
(defmeth continuous-family-proto :continuous? (&key &allow-other-keys)
  t)


;;; :density
(defmeth family-proto :density
  	(quantiles &key (parameters nil))
   (error "~A implementation does not have required :density method"
	  self))

(send family-proto :documentation :density
      "quantiles &key (parameters)
Returns density of distribution corresponding to <quantiles>
evaluated at <parameters>.
")

(defmeth discrete-family-proto :density
  		(quantiles &key (parameters nil))
   (uerror "Density of 0" "Discrete distributions have mass, not density")
   (- quantiles quantiles))		;should generate 0's same shape

;
;;;------------------------- Discrete Distributions ------------------

;; These methods are required for the proper handling of discrete
;; distributions. 

;;; :continuous?
(defmeth discrete-family-proto :continuous? (&key &allow-other-keys)
  nil)


;;; :rv-integer?
(defmeth continuous-family-proto :rv-integer? (&key &allow-other-keys)
  nil)


;;; :atoms
(defmeth family-proto :atoms
  	(&key (parameters nil) (minimum-mass .001))
   (error "~A implementation does not have required :atoms method"
	  self))

(send family-proto :documentation :atoms
      "&key parameters minimum-mass
Returns atoms of distribution evaluated at <parameters>
<minimum-mass> is used as a cut off point.
")

(defmeth continuous-family-proto :atoms
  	(&key (parameters nil) (minimum-mass .001))
  nil)


;;; :mass
(defmeth family-proto :mass
  	(&optional atoms &key (parameters nil))
   (error "~A implementation does not have required :mass method"
	  self))

(send family-proto :documentation :mass
      "&optional atoms &key parameters 
Returns mass of distribution at <atmos> evaluated at <parameters>.
")

(defmeth continuous-family-proto :mass
  		(quantiles &key (parameters nil))
   (uerror "Mass of 0" "Continuous distributions have density, not mass")
   (- quantiles quantiles))		;should generate 0's same shape




;

(defvar *Known-families* (list Undefined-family)
  "List of Known distribution families")
(defvar *Known-prior-families* ()
  "List of Known prior distribution families")


(new-provide :el-family)






