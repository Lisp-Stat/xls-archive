;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy and use this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the distribution family prototypes for an elicitation tool

(require :el-utils (strcat ElToY-directory "utils" *d-sep*  "utils.lsp"))
(require :el-update (strcat ElToY-directory "utils" *d-sep*  "update.lsp"))
;(require :el-paramixin (strcat ElToY-directory "params" *d-sep*  "paramixin.lsp"))
(require :el-family (strcat ElToY-directory "fam" *d-sep*  "family.lsp"))


;;; likelihood families


(defproto likelihood-family-proto
  '(
    nui-parameter-names			;nuisance parameter names
    nui-default-parameters		;default values for nuisance parameters
    nui-parameter-limits		;hard limits for nuisance parameter
					;values
    nui-parameter-range-default		;default range for nuisance parameters
    nui-parameter-granularity		;granularity of nuisance parameter
					;sliders
    nui-parameter-integer?		;constrain parameter to integer?
    nui-nparams				;number of nuisance parameters
    data-constraint-fun			;constraint function for data
    nui-parameter-constraint-fun	;constraint function for
					;nuisance parameters
    ) '() (list family-proto)
"Extras needed for likelihood family should have :data-default and
:data-limits method"
)


;;; set /selector methods

(defmeth likelihood-family-proto :nui-parameter-names
  	(&optional (parameter-names nil set))
   (when set
	 (unless (eql (list-length parameter-names) (slot-value 'nui-nparams))
		 (error "family:names Bad names ~S" parameter-names))
	 (setf (slot-value 'nui-parameter-names) parameter-names))
   (slot-value 'nui-parameter-names))

(send likelihood-family-proto :documentation :nui-parameter-names
 "Returns/sets nuisance parameter names."
)

(defmeth likelihood-family-proto :nui-default-parameters
  	(&optional (default-parameters nil set))
   (when set 
	 (unless (and (eql (slot-value 'nui-nparams)
			   (list-length default-parameters))
		      (every #'numberp default-parameters))
		 (error "family::default-parameters --- bad values: ~S"
			default-parameters))
	 (setq default-parameters
	       (round-if default-parameters
			 (slot-value :nui-parameter-integer?))) 
	 (unless (every #'between default-parameters
			          (slot-value 'nui-parameter-limits))
		 (uerror "Bring within limits "
			 "family::default-parameters --- values outside hard limits: ~S ~S"
			 default-parameters
			 (slot-value 'nui-parameter-limits)))
	 (unless (funcall (send self :nui-parameter-constraint-fun)
			default-parameters :warn t)
		 (error "family::nui-default-parameters --- Unconstrained values: ~S"
			default-parameters))
 	 (if (every #'between default-parameters
		              (slot-value 'nui-parameter-range-default))
	     (setf (slot-value 'nui-default-parameters) default-parameters)
	   (let* ((newvalrange
		   (force-within default-parameters
				 (slot-value 'nui-parameter-range-default)
				 (slot-value 'nui-parameter-limits))))
	     (setf (slot-value 'nui-default-parameters)
		   (mapcar #'car newvalrange))
	     (send self :parameter-range-default (mapcar #'cdr newvalrange)))))
   (slot-value 'nui-default-parameters))

(send likelihood-family-proto :documentation :nui-default-parameters
 "Returns/sets nuisance parameter default values.
Checks consistency among values, range, limits and integer constraints.
"
)


(defmeth likelihood-family-proto :nui-parameter-limits
  	(&optional (parameter-limits nil set))
   (when set 
	 (unless (and (eql (slot-value 'nui-nparams)
			   (list-length parameter-limits))
		      (every #'test-pair parameter-limits))
		 (error "isnew::parameter-limits --- bad limits: ~S"
			parameter-limits))
	 (setq parameter-limits
	       (round-if parameter-limits
			 (slot-value 'nui-parameter-integer?))) 
	 (setf (slot-value 'nui-parameter-limits) parameter-limits)
 	 (unless (every #'between2 (slot-value 'nui-parameter-range-default)
			           parameter-limits)
		 (send self :parameter-range-default 
		       (mapcar #'force2-between
			       (slot-value 'nui-parameter-range-default)
			       parameter-limits))))
   (slot-value 'nui-parameter-limits))

(send likelihood-family-proto :documentation :nui-parameter-limits
 "Returns/sets nuisance parameter hard limits.
Checks consistency among values, range, limits and integer constraints.
"
)


(defmeth likelihood-family-proto :nui-parameter-range-default
  	(&optional (parameter-range-default nil set))
   (when set 
	 (unless (and (eql (slot-value 'nui-nparams)
			   (list-length parameter-range-default))
		      (every #'test-pair parameter-range-default))
		 (error "family::range --- bad range: ~S"
			parameter-range-default))
	 (setq parameter-range-default
	       (round-if parameter-range-default
			 (slot-value 'nui-parameter-integer?)))
	 (unless (every #'between2 parameter-range-default
			(slot-value 'nui-parameter-limits))
		 (uerror "Bring within limits "
			 "family::range --- range outside hard parameter-limits: ~S ~S" parameter-range-default
			 (slot-value 'nui-parameter-limits)))
 	 (if (every #'between2 parameter-range-default
		    (slot-value 'nui-parameter-limits))
	     (setf (slot-value 'nui-parameter-range-default)
		   parameter-range-default)
	   (setq parameter-range-default
		 (setf (slot-value 'nui-parameter-range-default)
		       (mapcar #'force2-between parameter-range-default
			       (slot-value 'nui-parameter-limits)))))
	 (unless (every #'between (slot-value 'nui-default-parameters)
			parameter-range-default)
		 (send self :default-parameters
		       (force-between (slot-value 'nui-default-parameters)
				      parameter-range-default))))
   (slot-value 'nui-parameter-range-default))

(send likelihood-family-proto :documentation :nui-parameter-range-default
 "Returns/sets nuisance parameter default elicitation range.
Checks consistency among values, range, limits and integer constraints.
"
)


(defmeth likelihood-family-proto :nui-parameter-granularity
  	(&optional (parameter-granularity nil set))
  (when set
	(unless (listp parameter-granularity)
		(setq parameter-granularity (list parameter-granularity)))
	(setq parameter-granularity
	      (ceiling-if parameter-granularity
			  (slot-value 'nui-parameter-integer?)))
	(if (eql (list-length parameter-granularity) 1)
	    (setq parameter-granularity
		  (make-list (slot-value 'nui-nparams)
			     :initial-element (car parameter-granularity))))
	(unless (and (eql (list-length parameter-granularity)
			  (slot-value 'nui-nparams))
		     (every #'numberp parameter-granularity))
		(error "family::granularity --- bad granularity: ~S"
		       parameter-granularity))
	(setf (slot-value 'nui-parameter-granularity) parameter-granularity))
  (slot-value 'nui-parameter-granularity))

(send likelihood-family-proto :documentation :nui-parameter-granularity
 "Returns/sets nuisance parameter default elicitation granularity.
"
)


(defmeth likelihood-family-proto :nui-parameter-integer?
  (&optional (integer? nil set))
   (when set
	 (unless (eql (list-length integer?) (slot-value 'nui-nparams))
		 (error "para:integer? Bad integer? ~S" integer?))
	 (setf (slot-value 'nui-parameter-integer?) integer?)
	 (send self :nui-parameter-limits
	       (round-if (slot-value 'nui-parameter-limits) integer?))
	 (send self :nui-parameter-range-default
	       (round-if (slot-value 'nui-parameter-range-default) integer?))
	 (send self :nui-default-parameters
	       (round-if (slot-value 'nui-default-parameters) integer?))
	 (send self :nui-parameter-granularity
	       (ceiling-if (slot-value 'nui-parameter-granularity) integer?)))
   (slot-value 'nui-parameter-integer?))

(send likelihood-family-proto :documentation :nui-parameter-integer?
  "Returns/ sets integer constraint.  On set, forces
limits, range, values and granularity to be integers.
")


(defmeth likelihood-family-proto :nui-parameter-constraint-fun
                (&optional (new-fun nil set))
   (when set
	 (unless (functionp new-fun)
		 (error "family::nui-parameter-constraint-fun Bad function ~S"
			new-fun)) 
	 (setf (slot-value 'nui-parameter-constraint-fun) new-fun))
   (slot-value 'nui-parameter-constraint-fun))

(send likelihood-family-proto :documentation :nui-parameter-constraint-fun
      "Returns/Sets additional constraint function for nuisance parameters.

Parameter constraint functions should take a list of parameters and an
optional :warn keyword arg (default t).  They may take additional
keyword args. They should return t if the constraints among the
parameters are satisfied and nil otherwise.  If :warn is t, they
should print a warning message describing the error.
")

(defmeth likelihood-family-proto :data-constraint-fun
                (&optional (new-fun nil set))
   (when set
	 (unless (functionp new-fun)
		 (error "family::data-constraint-fun Bad function ~S"
			new-fun)) 
	 (setf (slot-value 'data-constraint-fun) new-fun))
   (slot-value 'data-constraint-fun))

(send likelihood-family-proto :documentation :data-constraint-fun
      "Returns/Sets additional constraint function for nuisance parameters.

Data constraint functions should take a list of data and a list of
nuisance parameters and an optional :warn keyword arg (default t).
They may take additional keyword args. They should return t if the
constraints among the parameters are satisfied and nil otherwise.  If
:warn is t, they should print a warning message describing the error.

As Data and nuisance parameters can possible be vectorized they should
behave correctly in those cases.
")



;;; :isnew initialization method

(defmeth likelihood-family-proto :isnew
	 (&rest args 
	  &key prior
	       (nui-parameter-names '())
	       (nui-default-parameters '()) 
	       (nui-parameter-range-default '() )
	       (nui-parameter-limits '())
	       (nui-parameter-integer? (list nil))
	       (nui-parameter-granularity '())
	       (nui-parameter-constraint-fun #'vacuous-constraint)
	       (data-constraint-fun #'vacuous-data-constraint)
	  &allow-other-keys)
  ;; deal with nuisance a parameters
	 
  (unless (listp nui-parameter-granularity)
	  (setq nui-parameter-granularity (list nui-parameter-granularity)))
  (unless (listp nui-parameter-integer?)
	  (setq nui-parameter-integer? (list nui-parameter-integer?)))

  (setf (slot-value 'nui-nparams)
	 (max (list-length nui-default-parameters)
	      (list-length nui-parameter-names)
	      (list-length nui-parameter-range-default)
	      (list-length nui-parameter-limits)
	      (list-length nui-parameter-granularity)
	      (list-length nui-parameter-integer?)))
  (if (null nui-parameter-integer?) (setq nui-parameter-integer? (list nil)))
  (if (eql 1 (list-length nui-parameter-integer?))
      (setq nui-parameter-integer?
	    (repeat nui-parameter-integer? (slot-value 'nui-nparams)))) 
  
  (unless (and (eql (slot-value 'nui-nparams)
		    (list-length nui-default-parameters))
	       (every #'checknump nui-default-parameters
		      nui-parameter-integer?))
	  (error "family::isnew --- bad nuisance default values: ~S~%"
		 nui-default-parameters))

  (unless (eql (slot-value 'nui-nparams) (list-length nui-parameter-names))
	  (error "family::isnew --- bad nuisance names: ~S"
		 nui-parameter-names)) 

  (unless (null nui-parameter-range-default)
      (unless (and (eql (slot-value 'nui-nparams)
			(list-length nui-parameter-range-default))
		   (every #'test-pair nui-parameter-range-default
			  nui-parameter-integer?))
	      (error "familty::isnew --- bad nuisance range-default: ~S"
		     nui-parameter-range-default))
      (unless (every #'between nui-default-parameters
		     nui-parameter-range-default)
	      (error "family::isnew --- nuisance default-parameters not between range: ~S ~S"
		     nui-default-parameters nui-parameter-range-default)))

  (if (null nui-parameter-limits)
      (setq nui-parameter-limits (make-list (slot-value 'nui-nparams)
					    :initial-element
					    *default-parameter-limits*))
    (unless (and (eql (slot-value 'nui-nparams)
		      (list-length nui-parameter-limits))
		 (every #'test-pair nui-parameter-limits
			nui-parameter-integer?))
	    (error "family::isnew --- bad nuisance limits: ~S"
		   nui-parameter-limits)))
  (unless (every #'between nui-default-parameters nui-parameter-limits)
	  (error "family::isnew --- nuisance default-parameters not between limits: ~S ~S"
		 nui-default-parameters nui-parameter-limits))
  (unless (or (null nui-parameter-range-default)
	      (every #'between2 nui-parameter-range-default
		     nui-parameter-limits))
	      (error "family::isnew --- range not between nuisance parameter-limits: ~S ~S"
		     nui-parameter-range-default nui-parameter-limits))

  (if (null nui-parameter-range-default)
       (setq nui-parameter-range-default nui-parameter-limits))

  ;;test constraint function satisfied.

  (unless (funcall nui-parameter-constraint-fun nui-default-parameters :warn t)
	  (error "family::isnew --- Unconstrained nuisance parameters: ~S"
		 default-parameters))
	  

   ;granularity is a system-wide default
   (if (null nui-parameter-granularity)
       (setq nui-parameter-granularity (list *default-granularity*)))

   ;we're ok, 
   ;set local parameters
   (setf (slot-value 'nui-parameter-integer?) nui-parameter-integer?)
   (setf (slot-value 'nui-parameter-constraint-fun)
	 nui-parameter-constraint-fun)
   (setf (slot-value 'data-constraint-fun) data-constraint-fun)
   (setf (slot-value 'nui-parameter-names) nui-parameter-names)
   (setf (slot-value 'nui-default-parameters) nui-default-parameters)
   (setf (slot-value 'nui-parameter-range-default) nui-parameter-range-default)
   (setf (slot-value 'nui-parameter-limits) nui-parameter-limits)
   (send self :nui-parameter-granularity nui-parameter-granularity)
   ; and get the rest
   (apply #'call-next-method args))

(send likelihood-family-proto :documentation :isnew
      (strcat
       (send family-proto :documentation :isnew)
       "Likelihood specific keywords:

   :nui-parameter-names (nuisance parameter names)
   :nui-default-parameters :nui-parameter-range-default
   :nui-parameter-integer? (integer constraint flags)
   :nui-parameter-granularity (defaults for elicitiation)
   :nui-parameter-limits (hard limits)
   :prior prior-family---not stored but could be used to calculate
          defaults. 
   :nui-parameter-constraint-fun --- function describing constraints
          among nuisance parameters 
   :data-constraint-fun --- function describing constraints on data
depending on nuisance parameters.  
"))



;; comprehensible printout
(send likelihood-family-proto :name :likelihood)

; :describe method --- verbose argument controls amount of excess
; printing done (t or integer).
(defmeth likelihood-family-proto :describe (&optional (stream t) (verbose 1))
  (call-next-method stream verbose)
  (format stream "The Nuisance Parameters are: ~S~%"
	   (send self :nui-parameter-names))
  (when (or (eq verbose t)
	    (and (numberp verbose) (>= verbose 1)))
	(format stream "Nuisance Parameter default values ~S~%"
		       (send self :nui-default-parameters))
	(format stream "Hard limits of nuisance parameter-values ~S~%"
		       (send self :nui-parameter-limits))
	(format stream "Integer constraints on nuisance parameter-values~S~%"
		(send self :nui-parameter-integer?))
	(when (or (eq verbose t)
		  (and (numberp verbose) (>= verbose 2)))
	      (format stream "Default range of nuisance parameter values ~S~%"
		      (send self :nui-parameter-range-default))
	      (when (or (eq verbose t)
			(and (numberp verbose) (>= verbose 3)))
		    (format stream "Granularity of nuisance parameter values ~S~%"
			    (send self :nui-parameter-granularity))))))


;;; :destruct

(defmeth likelihood-family-proto :destruct (&rest args)
  (setf (slot-value 'nui-parameter-names) nil)
  (setf (slot-value 'nui-default-parameters) nil)
  (setf (slot-value 'nui-parameter-limits) nil)
  (setf (slot-value 'nui-parameter-range-default) nil)
  (setf (slot-value 'nui-parameter-granularity) nil)
  (setf (slot-value 'nui-parameter-integer?) nil)
  (setf (slot-value 'nui-parameter-constraint-fun) nil)
  (setf (slot-value 'data-constraint-fun) nil)
  (setf (slot-value 'nui-nparams) nil)
  (apply #'call-next-method args))


;
;;; ---- Undefined-likelihood-family ----

;;; Undefined-likelihood-family --- this is the default for several uses.
(defproto Undefined-likelihood-family
  '() '()
  (list likelihood-family-proto Undefined-family)
"Undefined family of the likelihood type"
)




(defmeth Undefined-likelihood-family :isnew (&rest args
				  &key (name (gensym "Undefined-likelihood-family"))
				  &allow-other-keys)
  (apply #'call-next-method :name name args))


;; to get things to print right.
(send Undefined-likelihood-family :name :|Undefined-likelihood|)

;;; :distribution maniputlation function


;
;;; ----- Discrete and Continuous Speicial cases.


(defproto discrete-likelihood-proto '() '()
  (list likelihood-family-proto discrete-family-proto)
  "Discrete probabilty distribution specialization")

(send discrete-likelihood-proto :name :|Disc-Likelihood|)


;;; --- continuous-family-proto ---

(defproto continuous-likelihood-proto '() '()
  (list likelihood-family-proto continuous-family-proto)
  "Discrete probabilty distribution specialization")

;; to get things to print right.
(send continuous-likelihood-proto :name :|Cont-Likelihood|)



(defvar *Known-likelihood-families* (list Undefined-likelihood-family)
  "List of Known likelihood distribution families")

(new-provide :el-likelihood)
