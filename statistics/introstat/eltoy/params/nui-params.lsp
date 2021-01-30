;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the objects for parameter manipulation


(require :el-utils (strcat ElToY-directory "utils" *d-sep*  "utils.lsp"))
(require :el-update (strcat ElToY-directory "utils" *d-sep*  "update.lsp"))
(require :el-parameters (strcat ElToY-directory "params" *d-sep*  "parameters.lsp"))


    
;
;;;; nui-parameter-mixin routines common to objects which have
;;;; nui-parameters 

(defproto nui-parameter-mixin
  '(nui-parameters) '() (list ElToY-object)
"This object creates a group of prior or posterior nui-parameters"
)

;; :isnew --- this sets the nui-parameters
(defmeth nui-parameter-mixin :isnew
  	(&rest args
	 &key  (nui-parameter-values nil) (nui-parameter-names nil)
	       (nui-parameter-range nil) (nui-parameter-limits nil)
	       (nui-parameter-granularity nil) (nui-parameter-integer? nil)
	       (nui-parameter-constraint-fun vacuous-constraint))
  (setf (slot-value 'nui-parameters)
	(send para-proto :new
	      :names nui-parameter-names
	      :values nui-parameter-values
	      :limits nui-parameter-limits
	      :range nui-parameter-range
	      :granularity nui-parameter-granularity
	      :integer? nui-parameter-integer?
	      :constraint-fun nui-parameter-constraint-fun))
  (apply #'call-next-method args))


(send nui-parameter-mixin :documentation :isnew
      "Nui-Parameter-mixin:  
     :nui-parameter-names   sets nui-parameter names
     :nui-parameter-values  sets nui-parameter values
     :nui-parameter-limits  sets nui-parameter limits
     :nui-parameter-range   sets nui-parameter ranges
     :nui-parameter-granularity sets nui-parameter input granularity
     :nui-parameter-integer? sets parameter integer constraint flag
     :nui-parameter-constraint-fun sets constraint function
")


;;; :destruct --- disposes of the nui-parameters
(defmeth nui-parameter-mixin :destruct (&rest args)
  (send (slot-value 'nui-parameters) :destruct)
  (setf (slot-value 'nui-parameters) nil)
  (apply #'call-next-method args))


;; slot setting/accessing methods

(defmeth nui-parameter-mixin :nui-parameter-names
  	(&optional (nui-parameter-names nil set))
   (when set (send self :nui-parameters :names nui-parameter-names))
   (if (slot-value 'nui-parameters)
       (send (slot-value 'nui-parameters) :names)
     nil))

;; nui-parameter methods: :nui-parameters (:value :rescale :limits
;; :describe :names :range :granularity)
;; signal could be :local, in which case the rest of the message is
;; interpreted as a set to the nui-parameters, one of the valid
;; signals for the nui-parameter or omitted in which case it defaults
;; to values
(defmeth nui-parameter-mixin :nui-parameters
  	(&optional (signal :values) &rest args)
  (when (find :nui-parameters (trace)) (format t "Object: ~S~%" self))
  (when (and (listp signal) (every #'numberp signal))
	(push signal args) (setq signal :values))
  (if (eq signal :local)		;local set of some nature
      (progn
	(if (find :nui-parameters (trace))
	    (format t ":nui-parameters (nui-parameter-mixin) send to nui-parameter~%"))
	(apply #'send (slot-value 'nui-parameters) args))
    (let ((set (if (member signal (send self :nui-parameter-names))
		   (or (> (list-length args) 1) (numberp (car args)))
		 (and (not (eql signal :describe)) args))))
      (if set
	  (apply #'send  self :update :nui-parameters signal args)
	(progn
	  (if (find :nui-parameters (trace))
	      (format t ":nui-parameters (nui-parameter-mixin) <set>=nil send to nui-parameter~%"))
	  (apply #'send  (slot-value 'nui-parameters) signal args)))))) 

  

(send nui-parameter-mixin :documentation :nui-parameters
"Usage: :nui-parameters [:local] <signal> &rest <args>.
     Behavior depends on value of <signal>:
     :values (or omitted signal) --- Returns/sets values
     :limits --- Returns/sets limits
     :names --- Returns/sets names
     :range --- Returns/sets range
     :granularity --- Returns/sets granularity
     :integer? Returns/sets integer constraint flag
     :constraint-fun --- Returns/sets constraint function
     :describe --- sends a describe message to embeded nui-parameter object
     <nui-parameter name> ---  Returns/sets nui-parameter value 
                  or with an optional qualifier Returns/sets specific
                  nui-parameter information.

     For example, :nui-parameters 'mean --- returns the mean
        and  :nui-parameters 'mean :range '(-10 10)  --- sets the
        elicitation range for the mean.

If the :local flag is present, <signal> <args> is sent the
      nui-parameter object.  
If not, then the method trys to establish if this is an attempt to <set>
the nui-parameters.  Rule used:
     <set> --- if <signal> is a nui-parameter name 
                  then <set> <-T if (length <args>) > 1 [more than just signal]
                                 or (car <args>) is number
               else if <signal> is not :describe
                    then <set> <- T if (length <args>) >0
               else <set> <- nil
If <set> is true an appropriate :update message is generated.
Otherwise, <signal> <args> is sent to nui-parameter object.
")

  
(defmeth nui-parameter-mixin :nui-parameter-object (&rest args)
  (if args
      (if (eql :local (car args))
	  (setf (slot-value 'nui-parameters) (cadr args))
	(apply #'send self :update :nui-parameter-object args))
    (slot-value 'nui-parameters)))
(send nui-parameter-mixin :documentation :nui-parameter-object
      "Returns/updates (sets with :local) actual nui-parameter object")



;
;;;; inh-nui-parameter-mixin routines common to objects which have
;;;; nui-parameters inhereited from a supertool

(defproto inh-nui-parameter-mixin
  '(nui-parameters) '() (list subtool-mixin ElToY-object)
"This object has reference to a group of nui-parameters inhereted from its
supertool.  

It has read access only to those nui-parameters.  To write the
nui-parameters, it sends the write message to its parent."
)

;; :isnew --- this sets the nui-parameters
(defmeth inh-nui-parameter-mixin :isnew
  	(&rest args
	 &key  (parent nil) (nui-parameters nil))
  (setf (slot-value 'nui-parameters) nui-parameters)
  (apply #'call-next-method args))

(send inh-nui-parameter-mixin :documentation :isnew
      (strcat
       "inh-nui-parameter-mixin:  :nui-parameters <para-object>
                                                  initialized object 
"
       (send subtool-mixin :documentation :isnew)))


;;; :destruct --- disposes of the nui-parameters
(defmeth inh-nui-parameter-mixin :destruct (&rest args)
  (setf (slot-value 'nui-parameters) nil)
  (apply #'call-next-method args))


;; slot setting/accessing methods

;;; nui-parameter-names getting method needed as base case for recursion
(defmeth inh-nui-parameter-mixin :nui-parameter-names
  	(&optional (nui-parameter-names nil set))
   (when set (send self :nui-parameters :names nui-parameter-names))
   (if (slot-value 'nui-parameters)
       (send (slot-value 'nui-parameters) :names)
     nil))
(send inh-nui-parameter-mixin :documentation :nui-parameter-names
      "Returns/sets nui-parameter names")


;; nui-parameter methods: :nui-parameters (:value :rescale :limits
;; :describe :names :range :granularity)
;; signal could be :local, which case it is passed to a local handler
;; if one exists
(defmeth inh-nui-parameter-mixin :nui-parameters
  	(&optional (signal :values) &rest args)
  (when (find :nui-parameters (trace)) (format t "Object: ~S~%" self))
  (when (and (listp signal) (every #'numberp signal))
	(push signal args) (setq signal :values))
  (if (eq signal :local)		;local set of some nature
      (let* ((signal (pop args))
	     (who (position signal (send self :nui-parameter-names))))
	(when (and (listp signal) (every #'numberp signal))
	      (push signal args) (setq signal :values))
	(if (send self :has-method signal)
	    (progn
	      	(if (find :nui-parameters (trace))
		    (format t ":nui-parameters (inh-nui-parameter-mixin) dispatching to signal ~S ~%"
			    signal))
		(apply #'send self signal :local args))
	  (when who
		(if (find :nui-parameters (trace))
		    (format t ":nui-parameters (inh-nui-parameter-mixin) ~Sth nui-parameter signal~%"
			    who))
		(apply #'send self :local-nui-parameters-dispatch
		       self who args))))
    ;; not a local call
    (let ((set (if (member signal (send self :nui-parameter-names))
		   (or (> (list-length args) 1) (numberp (car args)))
		 (and (not (eql signal :describe)) args))))
      (if set
	  (progn
	    	(if (find :nui-parameters (trace))
		    (format t ":nui-parameters (inh-nui-parameter-mixin) <set>=T passing to supertool~%"))
		(apply #'send  self :supertool :nui-parameters signal args))
	(progn
	  (if (find :nui-parameters (trace))
	      (format t ":nui-parameters (inh-nui-parameter-mixin) <set>=nil handling locally~%"))
	  (if (slot-value 'nui-parameters)
	      (apply #'send (slot-value 'nui-parameters) signal args)
	    nil))))))
  

(send inh-nui-parameter-mixin :documentation :nui-parameters
"Usage: :nui-parameters [:local] <signal> &rest <args>.
     Behavior depends on value of <signal>:
     :values (or omitted signal) --- Returns/sets values
     :limits --- Returns/sets limits
     :names --- Returns/sets names
     :range --- Returns/sets range
     :integer? Returns/sets integer constraint flag
     :granularity --- Returns/sets granularity
     :constraint-fun --- Returns/sets constraint function
     :describe --- sends a describe message to embeded nui-parameter object
     <nui-parameter name> ---  Returns/sets nui-parameter value 
                  or with an optional qualifier Returns/sets specific
                  nui-parameter information.

     For example, :nui-parameters 'mean --- returns the mean
        and  :nui-parameters 'mean :range '(-10 10)  --- sets the
        elicitation range for the mean.

If the :local flag is present, and <signal> is a valid message for the
   <self>,  object sends itself <signal> :local <args>.
If not, then the method trys to establish if this is an attempt to <set>
the nui-parameters.  Rule used:
     <set> --- if <signal> is a nui-parameter name 
                  then <set> <-T if (length <args>) > 1 [more than just signal]
                                 or (car <args>) is number
               else if <signal> is not :describe
                    then <set> <- T if (length <args>) >0
               else <set> <- nil
If <set> is true an appropriate :update message is generated.
Otherwise, <signal> <args> is sent to nui-parameter object.
")

(defmeth inh-nui-parameter-mixin :local-nui-parameters-dispatch
  		(who &rest args)
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


;; nui-parameter-object
(defmeth inh-nui-parameter-mixin :nui-parameter-object (&rest args)
  (if args
      (if (eql :local (car args))
	  (setf (slot-value 'nui-parameters) (cadr args))
	(apply #'send self :supertool :nui-parameter-object args))
    (slot-value 'nui-parameters)))
(send inh-nui-parameter-mixin :documentation :nui-parameter-object
      "Returns/updates (sets in parent) actual nui-parameter object")



(new-provide :el-nui-parameters)
