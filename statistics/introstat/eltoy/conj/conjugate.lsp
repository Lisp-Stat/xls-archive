;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy and use this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the distribution prior-likelihood prototypes for an
;;; elicitation tool 

(require :el-utils (strcat ElToY-directory "utils" *d-sep*  "utils.lsp"))
(require :el-family (strcat ElToY-directory "fam" *d-sep*  "family.lsp"))
(require :el-likelihood (strcat ElToY-directory "conj" *d-sep*  "likelihood.lsp"))
(require :el-prior (strcat ElToY-directory "conj" *d-sep*  "prior.lsp"))

;;;;;; conjugate-family prior and posterior distribution object

(defproto conjugate-family-proto
    '(prior-family			;prior distribution family
      likelihood-family			;likelihood family function
					;form
      ) '() (list ElToY-object)
"Prior and likelihood bundle"
)


; :print method
(defmeth conjugate-family-proto :print (&optional (stream t))
   (format stream "#<conjugate-family: (~S--~S)>"
	   (let ((prior-family (send self :prior-family)))
	     (if prior-family (send prior-family :name)))
	   (let ((likelihood-family (send self :likelihood-family)))
	     (if likelihood-family (send likelihood-family :name)))))

; :describe method
(defmeth conjugate-family-proto :describe (&optional (stream t)
						     (verbose 1))
   (format stream "A conjugate ~S--~S family~%"
	   (send (send self :prior-family) :name)
	   (send (send self :likelihood-family) :name))
   (send (send self :prior-family) :describe stream verbose)
   (send (send self :likelihood-family) :describe stream verbose))


; :name
(defmeth conjugate-family-proto :name ()
  (make-symbol (format nil ":~A--~A"
		       (send (send self :prior-family) :name)
		       (send (send self :likelihood-family) :name))))
(send conjugate-family-proto :documentation :name
      "Fetches family name.")


; :isnew method
(defmeth conjugate-family-proto :isnew
	 (&rest args 
	  &key (prior-family undefined-family)
	       (likelihood-family undefined-likelihood-family)
	       &allow-other-keys)

   (send self :prior-family (send prior-family :new))
   (send self :likelihood-family (send likelihood-family :new
				       :prior (send self :prior-family)))
   )

(send conjugate-family-proto :documentation :isnew
      "&key prior-family likelihood-family

Note: expects `family' (prototype) objects as arguments, sends each
one a `new' message.  Likelihood-family gets prior family as :prior
keyword argument.
")



; :destruct method
(defmeth conjugate-family-proto :destruct (&rest args)
  (send (slot-value 'prior-family) :destruct)
  (send (slot-value 'likelihood-family) :destruct)
  (setf (slot-value 'prior-family) nil)
  (setf (slot-value 'likelihood-family) nil)
  )


; :prior-family message  displays/sets prior-family
(defmeth conjugate-family-proto :prior-family
  (&optional (prior-family nil set))
  (if set (setf (slot-value 'prior-family) prior-family))
  (slot-value 'prior-family))

(send conjugate-family-proto :documentation :prior-family
      "Returns/sets prior family.")


; :likelihood-family message  displays/sets likelihood-family
(defmeth conjugate-family-proto :likelihood-family
  (&optional (likelihood-family nil set))
  (if set (setf (slot-value 'likelihood-family) likelihood-family))
  (slot-value 'likelihood-family))

(send conjugate-family-proto :documentation :likelihood-family
      "Returns/sets likelihood family.")

;; :parameter-names (sets both rv of prior and parameters of
;; likelihood)
(defmeth conjugate-family-proto :parameter-names
  (&optional (parameter-names nil set))
  (when set (send
	     (slot-value 'likelihood-family) :parameter-names parameter-names)
	(send (slot-value 'prior-family) :rv-names parameter-names))
  (send (slot-value 'likelihood-family) :parameter-names))

(send conjugate-family-proto :documentation :parameter-names
      "Returns parameter names.
Sets both likelihood parameter and prior rv names.")


;; :hyperparameter-names,  :data-names
;; :default-hyperparameters :hyperparameter-limits 
;; :hyperparameter-range-default 

(defmeth conjugate-family-proto :hyperparameter-names
  (&optional (parameter-names nil set))
  (when set (send (slot-value 'prior-family) :parameter-names parameter-names))
  (send (slot-value 'prior-family) :parameter-names))

(send conjugate-family-proto :documentation :hyperparameter-names
      "Returns/sets hyperparameter (prior parameter) names.")

(defmeth conjugate-family-proto :default-hyperparameters
  (&optional (default-parameters nil set))
  (when set (send (slot-value 'prior-family) :default-parameters
		  default-parameters))
  (send (slot-value 'prior-family) :default-parameters))

(send conjugate-family-proto :documentation :default-hyperparameters
      "Returns/sets hyperparameter (prior parameter) default values.")


(defmeth conjugate-family-proto :hyperparameter-limits
  (&optional (parameter-limits nil set))
  (when set (send (slot-value 'prior-family) :parameter-limits
		  parameter-limits)) 
  (send (slot-value 'prior-family) :parameter-limits))
(send conjugate-family-proto :documentation :hyperparameter-limits
      "Returns/sets hyperparameter (prior parameter) hard limits.")


(defmeth conjugate-family-proto :hyperparameter-integer?
  (&optional (parameter-integer? nil set))
  (when set (send (slot-value 'prior-family) :parameter-integer?
		  parameter-integer?)) 
  (send (slot-value 'prior-family) :parameter-integer?))
(send conjugate-family-proto :documentation :hyperparameter-integer?
      "Returns/sets hyperparameter (prior parameter) integer constraint.")

(defmeth conjugate-family-proto :hyperparameter-granularity
  (&optional (parameter-granularity nil set))
  (when set (send (slot-value 'prior-family) :parameter-granularity
		  parameter-granularity)) 
  (send (slot-value 'prior-family) :parameter-granularity))
(send conjugate-family-proto :documentation :hyperparameter-granularity
      "Returns/sets hyperparameter (prior parameter) elicitation granularity.")


(defmeth conjugate-family-proto :hyperparameter-range-default
  (&optional (parameter-range-default nil set))
  (when set (send (slot-value 'prior-family) :parameter-range-default
		  parameter-range-default))
  (send (slot-value 'prior-family) :parameter-range-default))
(send conjugate-family-proto :documentation :hyperparameter-range-default
      "Returns/sets hyperparameter (prior parameter) elicitation range.")

(defmeth conjugate-family-proto :hyperparameter-constraint-fun
  (&optional (parameter-constraint-fun nil set))
  (when set (send (slot-value 'prior-family) :parameter-constraint-fun
		  parameter-constraint-fun))
  (send (slot-value 'prior-family) :parameter-constraint-fun))
(send conjugate-family-proto :documentation :hyperparameter-constraint-fun
      "Returns/sets hyperparameter (prior parameter) constraint function.")



(defmeth conjugate-family-proto :data-names
  (&optional (data-names nil set))
  (when set (send (slot-value 'likelihood-family) :rv-names data-names))
  (send (slot-value 'likelihood-family) :rv-names))

(send conjugate-family-proto :documentation :data-names
      "Returns/sets data (likelihood r.v.) names.")


(defmeth conjugate-family-proto :default-parameters
  (&optional (default-parameters nil set))
  (when set (send (slot-value 'likelihood-family) :default-parameters
		  default-parameters))
  (send (slot-value 'prior-family) :rv-default))

(send conjugate-family-proto :documentation :default-parameters
      "Returns default (likelihood) parameters (calculated from prior).
Sets default likelihood parameters in likelihood.")


(defmeth conjugate-family-proto :parameter-limits
  (&optional (parameter-limits nil set))
  (when set (send (slot-value 'likelihood-family) :parameter-limits
		  parameter-limits)) 
  (send (slot-value 'likelihood-family) :parameter-limits))

(send conjugate-family-proto :documentation :parameter-limits
      "Returns/Sets (likelihood) parameters hard limits.
(calculated in likelihood).")


(defmeth conjugate-family-proto :parameter-range-default
  (&optional (parameter-range-default nil set))
  (when set (send (slot-value 'likelihood-family) :parameter-range-default
		  parameter-range-default))
  (send (slot-value 'likelihood-family) :parameter-range-default))

(send conjugate-family-proto :documentation :parameter-range-default
      "Returns/Sets (likelihood) parameters default-elicitation range.
(calculated in likelihood).")


(defmeth conjugate-family-proto :nui-parameter-names
  (&optional (nui-parameter-names nil set))
  (when set (send (slot-value 'likelihood-family) :nui-parameter-names
		  nui-parameter-names))
  (send (slot-value 'likelihood-family) :nui-parameter-names))

(send conjugate-family-proto :documentation :nui-parameter-names
      "Returns/Sets (likelihood) nuisance parameter names.
")



(defmeth conjugate-family-proto :nui-default-parameters
  (&optional (nui-default-parameters nil set))
  (when set (send (slot-value 'likelihood-family) :nui-default-parameters
		  nui-default-parameters))
  (send (slot-value 'likelihood-family) :nui-default-parameters))

(send conjugate-family-proto :documentation :nui-default-parameters
      "Returns/Sets (likelihood) nuisance parameter default values.
")


(defmeth conjugate-family-proto :nui-parameter-limits
  (&optional (parameter-limits nil set))
  (when set (send (slot-value 'likelihood-family) :nui-parameter-limits
		  nui-parameter-limits)) 
  (send (slot-value 'likelihood-family) :nui-parameter-limits))

(send conjugate-family-proto :documentation :nui-parameter-limits
      "Returns/Sets (likelihood) nuisance parameter hard limits.
")



(defmeth conjugate-family-proto :nui-parameter-range-default
  (&optional (nui-parameter-range-default nil set))
  (when set (send (slot-value 'likelihood-family) :nui-parameter-range-default
		  nui-parameter-range-default))
  (send (slot-value 'likelihood-family) :nui-parameter-range-default))
(send conjugate-family-proto :documentation :nui-parameter-range-default
      "Returns/Sets (likelihood) nuisance parameter default
elicitiation range.
")

(defmeth conjugate-family-proto :nui-parameter-integer?
  (&optional (parameter-integer? nil set))
  (when set (send (slot-value 'likelihood-family) :nui-parameter-integer?
		  nui-parameter-integer?)) 
  (send (slot-value 'likelihood-family) :nui-parameter-integer?))

(send conjugate-family-proto :documentation :nui-parameter-integer?
      "Returns/Sets (likelihood) nuisance parameter integer contraint.
")

(defmeth conjugate-family-proto :nui-parameter-granularity
  (&optional (parameter-granularity nil set))
  (when set (send (slot-value 'likelihood-family) :nui-parameter-granularity
		  nui-parameter-granularity)) 
  (send (slot-value 'likelihood-family) :nui-parameter-granularity))

(send conjugate-family-proto :documentation :nui-parameter-granularity
      "Returns/Sets (likelihood) nuisance parameter elicitation granularity.
")


(defmeth conjugate-family-proto :nui-parameter-constraint-fun
  (&optional (parameter-constraint-fun nil set))
  (when set (send (slot-value 'likelihood-family) :nui-parameter-constraint-fun
		  parameter-constraint-fun))
  (send (slot-value 'likelihood-family) :nui-parameter-constraint-fun))
(send conjugate-family-proto :documentation :nui-parameter-constraint-fun
      "Returns/sets nuisance parameter constraint function.")


(defmeth conjugate-family-proto :data-limits
  (&rest args)
  (apply #'send (slot-value 'likelihood-family) :rv-limits args))

(send conjugate-family-proto :documentation :data-limits
      "Returns data (likelihood r.v.) hard limits.
")

(defmeth conjugate-family-proto :data-integer?
  (&rest args)
  (apply #'send (slot-value 'likelihood-family) :rv-integer? args))

(send conjugate-family-proto :documentation :data-integer?
      "Returns data (likelihood r.v.) integer contstraint
")


(defmeth conjugate-family-proto :data-range-default (&rest args)
  (apply #'send (slot-value 'likelihood-family) :rv-range-default args))
(send conjugate-family-proto :documentation :data-limits
      "Returns data (likelihood r.v.) default elicititation range.
")


(defmeth conjugate-family-proto :default-data
  (&optional (default-data nil set))
  (send (slot-value 'likelihood-family) :rv-default))
(send conjugate-family-proto :documentation :data-limits
      "Returns data (likelihood r.v.) default elicititation range.
")


(defmeth conjugate-family-proto :data-constraint-fun
  (&optional (parameter-constraint-fun nil set))
  (when set (send (slot-value 'likelihood-family) :data-constraint-fun
		  parameter-constraint-fun))
  (send (slot-value 'likelihood-family) :data-constraint-fun))
(send conjugate-family-proto :documentation :data-constraint-fun
      "Returns/sets nuisance parameter constraint function.")



;;; :required methods for conjugate-families

;;; :forward-link --- returns posterior hyperparameters from prior
;;; hyperparameters 
(defmeth conjugate-family-proto :forward-link
  	(&rest args &key
	       parameters nui-parameters data
	       &allow-other-keys)
   (error "Forward-Link undefined for family ~A" self))

;;; :reverse-link --- returns posterior hyperparameters from prior
;;; hyperparameters 
(defmeth conjugate-family-proto :reverse-link
  	(&rest args &key
	       parameters nui-parameters data
	       &allow-other-keys)
  (error "`Reverse-Link undefined for family ~A" self))





;
;;;;;; Unknown-conjugate-family

(defproto Undefined-conjugate-family '()
  '() (list conjugate-family-proto)
"Undefined conjugate family"
)


(send Undefined-conjugate-family :slot-value 'likelihood-family
      Undefined-likelihood-family)

(send Undefined-conjugate-family :slot-value 'prior-family
      Undefined-prior-family)

(defvar *Known-conjugate-families* (list Undefined-conjugate-family)
  "List of Known conjugate distribution families")


(new-provide :el-conjugate)

