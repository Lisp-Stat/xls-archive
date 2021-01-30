;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the objects for parameter manipulation


(require :el-utils (strcat ElToY-directory "utils" *d-sep*  "utils.lsp"))
(require :el-update (strcat ElToY-directory "utils" *d-sep*  "update.lsp"))
(require :el-parameters (strcat ElToY-directory "params" *d-sep*  "parameters.lsp"))

;;;; parameter-mixin routines common to objects which have
;;;; parameters 

(defproto parameter-mixin
  '(parameters) '() (list ElToY-object)
"This object creates a group of prior or posterior parameters"
)

;; :isnew --- this sets the parameters
(defmeth parameter-mixin :isnew
  	(&rest args
	 &key  (parameter-values nil) (parameter-names nil)
	       (parameter-range nil) (parameter-limits nil)
	       (parameter-granularity nil) (parameter-integer? nil)
	       (parameter-constraint-fun vacuous-constraint))
	  
  (setf (slot-value 'parameters)
	(send para-proto :new
	      :names parameter-names
	      :values parameter-values
	      :limits parameter-limits
	      :range parameter-range
	      :granularity parameter-granularity
	      :integer? parameter-integer?
	      :constraint-fun parameter-constraint-fun))
  (apply #'call-next-method args))

(send parameter-mixin :documentation :isnew
      "Parameter-mixin:  
     :parameter-names   sets parameter names
     :parameter-values  sets parameter values
     :parameter-limits  sets parameter limits
     :parameter-range   sets parameter ranges
     :parameter-granularity sets parameter input granularity
     :parameter-integer? sets parameter integer constraint flag
     :parameter-constraint-fun sets parameter constraint function
")


;;; :destruct --- disposes of the parameters
(defmeth parameter-mixin :destruct (&rest args)
  (send (slot-value 'parameters) :destruct)
  (setf (slot-value 'parameters) nil)
  (apply #'call-next-method args))


;; slot setting/accessing methods

;;; parameter-names getting method needed as base case for recursion
(defmeth parameter-mixin :parameter-names
  	(&optional (parameter-names nil set))
   (when set (send self :parameters :names parameter-names))
   (send (slot-value 'parameters) :names))
(send parameter-mixin :documentation :parameter-names
"Returns/sets parameter names."
)


;; parameter methods: :parameters (:values :limits
;; :describe :names :range :granularity)
;; signal could be :local, in which case the rest of the message is
;; interpreted as a set to the parameters, one of the valid
;; signals for the parameter or omitted in which case it defaults
;; to values
(defmeth parameter-mixin :parameters
  	(&optional (signal :values) &rest args)
  (when (find :parameters (trace)) (format t "Object: ~S~%" self))
  (when (and (listp signal) (every #'numberp signal))
	(push signal args) (setq signal :values))
  (if (eq signal :local)		;local set of some nature
      (progn
	(if (find :parameters (trace))
	    (format t ":parameters (parameter-mixin) send to parameter~%"))
	(apply #'send (slot-value 'parameters) args))
    (let ((set (if (member signal (send self :parameter-names))
		   (or (> (list-length args) 1) (numberp (car args)))
		 (and (not (eql signal :describe)) args))))
      (if set
	  (apply #'send  self :update :parameters signal args)
	(progn
	  (if (find :parameters (trace))
	      (format t ":parameters (parameter-mixin) <set>=nil send to parameter~%"))
	  (apply #'send  (slot-value 'parameters) signal args))))))

(send parameter-mixin :documentation :parameters
"Usage: :parameters [:local] <signal> &rest <args>.
     Behavior depends on value of <signal>:
     :values (or omitted signal) --- Returns/sets values
     :limits --- Returns/sets limits
     :names --- Returns/sets names
     :range --- Returns/sets range
     :granularity --- Returns/sets granularity
     :integer? Returns/sets integer constraint flag
     :constraint-fun --- Returns/sets constraint function
     :describe --- sends a describe message to embeded parameter object
     <parameter name> ---  Returns/sets parameter value 
                  or with an optional qualifier Returns/sets specific
                  parameter information.

     For example, :parameters 'mean --- returns the mean
        and  :parameters 'mean :range '(-10 10)  --- sets the
        elicitation range for the mean.

If the :local flag is present, <signal> <args> is sent the
      parameter object.  
If not, then the method trys to establish if this is an attempt to <set>
the parameters.  Rule used:
     <set> --- if <signal> is a parameter name 
                  then <set> <-T if (length <args>) > 1 [more than just signal]
                                 or (car <args>) is number
               else if <signal> is not :describe
                    then <set> <- T if (length <args>) >0
               else <set> <- nil
If <set> is true an appropriate :update message is generated.
Otherwise, <signal> <args> is sent to parameter object.
")


(defmeth parameter-mixin :parameter-object (&rest args)
  (if args
      (if (eql :local (car args))
	  (setf (slot-value 'parameters) (cadr args))
	(apply #'send self :update :parameter-object args))
    (slot-value 'parameters)))
(send parameter-mixin :documentation :parameter-object
      "Returns/updates (sets with :local) actual parameter object")



;
;;;; inh-parameter-mixin routines common to objects which have
;;;; parameters inhereited from a supertool

(defproto inh-parameter-mixin
  '(parameters) '() (list subtool-mixin ElToY-object)
"This object has reference to a group of parameters inhereted from its
supertool.  

It has read access only to those parameters.  To write the
parameters, it sends the write message to its parent."
)

;; :isnew --- this sets the parameters
(defmeth inh-parameter-mixin :isnew
  	(&rest args
	 &key  (parameters nil)
	 &allow-other-keys)
  (setf (slot-value 'parameters) parameters)
  (apply #'call-next-method args))

(send inh-parameter-mixin :documentation :isnew
      (strcat
       "inh-parameter-mixin:  :parameters <para-object> initialized object
"
       (send subtool-mixin :documentation :isnew)))


;;; :destruct --- disposes of the parameters
(defmeth inh-parameter-mixin :destruct (&rest args)
  (setf (slot-value 'parameters) nil)
  (apply #'call-next-method args))


;; slot setting/accessing methods

;;; parameter-names getting method needed as base case for recursion
(defmeth inh-parameter-mixin :parameter-names
  	(&optional (parameter-names nil set))
   (when set (send self :parameters :names parameter-names))
   (send (slot-value 'parameters) :names))
(send inh-parameter-mixin :documentation :parameter-names
      "Returns/sets parameter names")


;; parameter methods: :parameters (:value :rescale :limits
;; :describe :names :range :granularity)
;; signal could be :local, which case it is passed to a local handler
;; if one exists
(defmeth inh-parameter-mixin :parameters
  	(&optional (signal :values) &rest args)
  (when (find :parameters (trace)) (format t "Object: ~S~%" self))
  (when (and (listp signal) (every #'numberp signal))
	(push signal args) (setq signal :values))
  (if (eq signal :local)		;local set of some nature
      (let* ((signal (pop args))
	     (who (position signal (send self :parameter-names))))
	(when (and (listp signal) (every #'numberp signal))
	      (push signal args) (setq signal :values))
	(if (send self :has-method signal)
	    (progn
	      	(if (find :parameters (trace))
		    (format t ":parameters (inh-parameter-mixin) dispatching to signal ~S ~%"
			    signal))
		(apply #'send self signal :local args))
	  (when who
		(if (find :parameters (trace))
		    (format t ":parameters (inh-parameter-mixin) ~Sth parameter signal~%"
			    who))
		(apply #'send self :local-parameters-dispatch
		       self who args))))
    ;; not a local call
    (let ((set (if (member signal (send self :parameter-names))
		   (or (> (list-length args) 1) (numberp (car args)))
		 (and (not (eql signal :describe)) args))))
      (if set
	  (progn
	    	(if (find :parameters (trace))
		    (format t ":parameters (inh-parameter-mixin) <set>=T passing to supertool~%"))
		(apply #'send  self :supertool :parameters signal args))
	(progn
	  (if (find :parameters (trace))
	      (format t ":parameters (inh-parameter-mixin) <set>=nil handling locally~%"))
	  (apply #'send  (slot-value 'parameters) signal args))))))


(send inh-parameter-mixin :documentation :parameters
"Usage: :parameters [:local] <signal> &rest <args>.
     Behavior depends on value of <signal>:
     :values (or omitted signal) --- Returns/sets values
     :limits --- Returns/sets limits
     :names --- Returns/sets names
     :range --- Returns/sets range
     :granularity --- Returns/sets granularity
     :integer? Returns/sets integer constraint flag
     :constraint-fun --- Returns/sets constraint function
     :describe --- sends a describe message to embeded parameter object
     <parameter name> ---  Returns/sets parameter value 
                  or with an optional qualifier Returns/sets specific
                  parameter information.

     For example, :parameters 'mean --- returns the mean
        and  :parameters 'mean :range '(-10 10)  --- sets the
        elicitation range for the mean.

If the :local flag is present, and <signal> is a valid message for the
   <self>,  object sends itself <signal> :local <args>.
If not, then the method trys to establish if this is an attempt to <set>
the parameters.  Rule used:
     <set> --- if <signal> is a parameter name 
                  then <set> <-T if (length <args>) > 1 [more than just signal]
                                 or (car <args>) is number
               else if <signal> is not :describe
                    then <set> <- T if (length <args>) >0
               else <set> <- nil
If <set> is true an appropriate :update message is generated.
Otherwise, <signal> <args> is sent to parameter object.
")




(defmeth inh-parameter-mixin :local-parameters-dispatch (who &rest args)
  (let* ((signal (if (numberp (car args)) :values1
		  (car args)))
	 (sig1 (case signal
		     ((:value :values) :values1)
		     (:range :range1)
		     (:granularlity :granularity1)
		     ((:integer :integer?) :integer?1)
		     ((:name :names) :names1)
		     (t signal))))
    (if (send self :has-method sig1)
	(apply #'send self sig1 who :local (cdr args)))))


(defmeth inh-parameter-mixin :parameter-object (&rest args)
  (if args
      (if (eql :local (car args))
	  (setf (slot-value 'parameters) (cadr args))
	(apply #'send self :supertool :parameter-object args))
    (slot-value 'parameters)))
(send inh-parameter-mixin :documentation :parameter-object
      "Returns/updates (sets in parent) actual parameter object")

    





(new-provide :el-paramixin)
