;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the objects for parameter manipulation


(require :el-utils (strcat ElToY-directory "utils" *d-sep*  "utils.lsp"))
(require :el-update (strcat ElToY-directory "utils" *d-sep*  "update.lsp"))
(require :el-parameters (strcat ElToY-directory "params" *d-sep*  "parameters.lsp"))



;;;; hyperparameter-mixin routines common to objects which have
;;;; hyperparameters 

(defproto hyperparameter-mixin
  '(hyperparameters) '() (list ElToY-object)
"This object creates a group of prior or posterior hyperparameters"
)

;; :isnew --- this sets the hyperparameters
(defmeth hyperparameter-mixin :isnew
  	(&rest args
	 &key  (hyperparameter-values nil)
	       (hyperparameter-names nil)
	       (hyperparameter-range nil)
	       (hyperparameter-limits nil)
	       (hyperparameter-granularity nil)
	       (hyperparameter-integer? nil)
	       (hyperparameter-constraint-fun vacuous-constraint))
  (setf (slot-value 'hyperparameters)
	(send para-proto :new
	      :names  		hyperparameter-names
	      :values 		hyperparameter-values
	      :limits 		hyperparameter-limits
	      :range  		hyperparameter-range
	      :granularity 	hyperparameter-granularity
	      :integer? 	hyperparameter-integer?
	      :constraint-fun 	hyperparameter-constraint-fun))
  (apply #'call-next-method args))

(send hyperparameter-mixin :documentation :isnew
      "Hyperparameter-mixin:  
     :hyperparameter-names   sets hyperparameter names
     :hyperparameter-values  sets hyperparameter values
     :hyperparameter-limits  sets hyperparameter limits
     :hyperparameter-range   sets hyperparameter ranges
     :hyperparameter-granularity sets hyperparameter input granularity
     :hyperparameter-constraint-fun sets constraint function
     :hyperparameter-integer? sets parameter integer constraint flag
")



;;; :destruct --- disposes of the hyperparameters
(defmeth hyperparameter-mixin :destruct (&rest args)
  (send (slot-value 'hyperparameters) :destruct)
  (setf (slot-value 'hyperparameters) nil)
  (apply #'call-next-method args))


;; slot setting/accessing methods

(defmeth hyperparameter-mixin :hyperparameter-names
  	(&optional (hyperparameter-names nil set))
   (when set (send self :hyperparameters :names hyperparameter-names))
   (send (slot-value 'hyperparameters) :names))


(send hyperparameter-mixin :documentation :hyperparameter-names
      "Returns/Sets hyperparameter names
")


;; hyperparameter methods: :hyperparameters (:value :rescale :limits
;; :describe :names :range :granularity)
;; signal could be :local, in which case the rest of the message is
;; interpreted as a set to the hyperparameters, one of the valid
;; signals for the hyperparameter or omitted in which case it defaults
;; to values
(defmeth hyperparameter-mixin :hyperparameters
  	(&optional (signal :values) &rest args)
  (when (find :hyperparameters (trace)) (format t "Object: ~S~%" self))
  (when (and (listp signal) (every #'numberp signal))
	(push signal args) (setq signal :values))
  (if (eq signal :local)		;local set of some nature
      (progn
	(if (find :hyperparameters (trace))
	    (format t ":hyperparameters (hyperparameter-mixin) send to hyperparameter~%"))
	(apply #'send (slot-value 'hyperparameters) args))
    (let ((set (if (member signal (send self :hyperparameter-names))
		   (or (> (list-length args) 1) (numberp (car args)))
		 (and (not (eql signal :describe)) args))))
      (if set
	  (apply #'send  self :update :hyperparameters signal args)
	(progn
	  (if (find :hyperparameters (trace))
	      (format t ":hyperparameters (hyperparameter-mixin) <set>=nil send to hyperparameter~%"))
	  (apply #'send  (slot-value 'hyperparameters) signal args)))))) 

(send hyperparameter-mixin :documentation :hyperparameters
"Usage: :hyperparameters [:local] <signal> &rest <args>.
     Behavior depends on value of <signal>:
     :values (or omitted signal) --- Returns/sets values
     :limits --- Returns/sets limits
     :names --- Returns/sets names
     :range --- Returns/sets range
     :granularity --- Returns/sets granularity
     :integer? Returns/sets integer constraint flag
     :constraint-fun --- Returns/sets constraint function
     :describe --- sends a describe message to embeded hyperparameter object
     <hyperparameter name> ---  Returns/sets hyperparameter value 
                  or with an optional qualifier Returns/sets specific
                  hyperparameter information.

     For example, :hyperparameters 'mean --- returns the mean
        and  :hyperparameters 'mean :range '(-10 10)  --- sets the
        elicitation range for the mean.

If the :local flag is present, <signal> <args> is sent the
      hyperparameter object.  
If not, then the method trys to establish if this is an attempt to <set>
the hyperparameters.  Rule used:
     <set> --- if <signal> is a hyperparameter name 
                  then <set> <-T if (length <args>) > 1 [more than just signal]
                                 or (car <args>) is number
               else if <signal> is not :describe
                    then <set> <- T if (length <args>) >0
               else <set> <- nil
If <set> is true an appropriate :update message is generated.
Otherwise, <signal> <args> is sent to hyperparameter object.
")

  
(defmeth hyperparameter-mixin :hyperparameter-object (&rest args)
  (if args
      (if (eql :local (car args))
	  (setf (slot-value 'hyperparameters) (cadr args))
	(apply #'send self :update :hyperparameter-object args))
    (slot-value 'hyperparameters)))
(send hyperparameter-mixin :documentation :hyperparameter-object
      "Returns/updates (sets with :local) actual hyperparameter object")



;
;;;; inh-hyperparameter-mixin routines common to objects which have
;;;; hyperparameters inhereited from a supertool

(defproto inh-hyperparameter-mixin
  '(hyperparameters) '() (list subtool-mixin ElToY-object)
"This object has reference to a group of hyperparameters inhereted from its
supertool.  

It has read access only to those hyperparameters.  To write the
hyperparameters, it sends the write message to its parent."
)

;; :isnew --- this sets the hyperparameters
(defmeth inh-hyperparameter-mixin :isnew
  	(&rest args
	 &key  (parent nil) (hyperparameters nil))
  (setf (slot-value 'hyperparameters) hyperparameters)
  (apply #'call-next-method args))

(send inh-hyperparameter-mixin :documentation :isnew
      (strcat
       "inh-hyperparameter-mixin:  :hyperparameters <para-object>
                                                  initialized object 
"
       (send subtool-mixin :documentation :isnew)))


;;; :destruct --- disposes of the hyperparameters
(defmeth inh-hyperparameter-mixin :destruct (&rest args)
  (setf (slot-value 'hyperparameters) nil)
  (apply #'call-next-method args))


;; slot setting/accessing methods

;;; hyperparameter-names getting method needed as base case for recursion
(defmeth inh-hyperparameter-mixin :hyperparameter-names
  	(&optional (hyperparameter-names nil set))
   (when set (send self :hyperparameters :names hyperparameter-names))
   (send (slot-value 'hyperparameters) :names))
(send inh-hyperparameter-mixin :documentation :hyperparameter-names
      "Returns/sets hyperparameter names")


;; hyperparameter methods: :hyperparameters (:value :rescale :limits
;; :describe :names :range :granularity)
;; signal could be :local, which case it is passed to a local handler
;; if one exists
(defmeth inh-hyperparameter-mixin :hyperparameters
  	(&optional (signal :values) &rest args)
  (when (find :hyperparameters (trace)) (format t "Object: ~S~%" self))
  (when (and (listp signal) (every #'numberp signal))
	(push signal args) (setq signal :values))
  (if (eq signal :local)		;local set of some nature
      (let* ((signal (pop args))
	     (who (position signal (send self :hyperparameter-names))))
	(when (and (listp signal) (every #'numberp signal))
	      (push signal args) (setq signal :values))
	(if (send self :has-method signal)
	    (progn
	      	(if (find :hyperparameters (trace))
		    (format t ":hyperparameters (inh-hyperparameter-mixin) dispatching to signal ~S ~%"
			    signal))
		(apply #'send self signal :local args))
	  (when who
		(if (find :hyperparameters (trace))
		    (format t ":hyperparameters (inh-hyperparameter-mixin) ~Sth hyperparameter signal~%"
			    who))
		(apply #'send self :local-hyperparameters-dispatch
		       self who args))))
    ;; not a local call
    (let ((set (if (member signal (send self :hyperparameter-names))
		   (or (> (list-length args) 1) (numberp (car args)))
		 (and (not (eql signal :describe)) args))))
      (if set
	  (progn
	    	(if (find :hyperparameters (trace))
		    (format t ":hyperparameters (inh-hyperparameter-mixin) <set>=T passing to supertool~%"))
		(apply #'send  self :supertool :hyperparameters signal args))
	(progn
	  (if (find :hyperparameters (trace))
	      (format t ":hyperparameters (inh-hyperparameter-mixin) <set>=nil handling locally~%"))
	  (apply #'send  (slot-value 'hyperparameters) signal args)))))) 


(send inh-hyperparameter-mixin :documentation :hyperparameters
"Usage: :hyperparameters [:local] <signal> &rest <args>.
     Behavior depends on value of <signal>:
     :values (or omitted signal) --- Returns/sets values
     :limits --- Returns/sets limits
     :names --- Returns/sets names
     :range --- Returns/sets range
     :integer? Returns/sets integer constraint flag
     :granularity --- Returns/sets granularity
     :constraint-fun --- Returns/sets constraint function
     :describe --- sends a describe message to embeded hyperparameter object
     <hyperparameter name> ---  Returns/sets hyperparameter value 
                  or with an optional qualifier Returns/sets specific
                  hyperparameter information.

     For example, :hyperparameters 'mean --- returns the mean
        and  :hyperparameters 'mean :range '(-10 10)  --- sets the
        elicitation range for the mean.

If the :local flag is present, and <signal> is a valid message for the
   <self>,  object sends itself <signal> :local <args>.
If not, then the method trys to establish if this is an attempt to <set>
the hyperparameters.  Rule used:
     <set> --- if <signal> is a hyperparameter name 
                  then <set> <-T if (length <args>) > 1 [more than just signal]
                                 or (car <args>) is number
               else if <signal> is not :describe
                    then <set> <- T if (length <args>) >0
               else <set> <- nil
If <set> is true an appropriate :update message is generated.
Otherwise, <signal> <args> is sent to hyperparameter object.
")

(defmeth inh-hyperparameter-mixin :local-hyperparameters-dispatch
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

(defmeth inh-hyperparameter-mixin :hyperparameter-object (&rest args)
  (if args
      (if (eql :local (car args))
	  (setf (slot-value 'hyperparameters) (cadr args))
	(apply #'send self :supertool :hyperparameter-object args))
    (slot-value 'hyperparameters)))
(send inh-hyperparameter-mixin :documentation :hyperparameter-object
      "Returns/updates (sets in parent) actual hyperparameter object")




(new-provide :el-hyperparameters)
