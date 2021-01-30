
;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;;; el-data-mixin routines common to objects which have
;;;; data 





(require :el-nui-parameters (strcat ElToY-directory "params" *d-sep* 
				    "nui-params.lsp"))
(require :el-data (strcat ElToY-directory "params" *d-sep*  "data.lsp"))


;;; ------- el-data-mixin --- Owns data object --------


(defproto el-data-mixin
  '(data
    ) '()
    (list ElToY-object)
"This object owns a group of prior or posterior data. It should always
have either a inh-nui-parameter-mixin or a nui-parameter-mixin."
)

;;; ---- Birth and Death ------

;;; :isnew --- this sets the data
(defmeth el-data-mixin :isnew
  	(&rest args
	 &key  (data-values nil) (data-names nil)
	       (data-limits nil) (data-integer? nil)
	       (data-constraint-fun vacuous-data-constraint)
	       (nui-parameter-object nil))
  (apply #'call-next-method args)
  (unless nui-parameter-object
	  (setq nui-parameter-object (send self :nui-parameter-object)))
  (setf (slot-value 'data)
	(send el-data-proto :new
	      :supertool self
	      :names data-names
	      :data data-values
	      :limits data-limits
	      :integer? data-integer?
	      :constraint-fun data-constraint-fun
	      :nui-parameters nui-parameter-object))
  )

(send el-data-mixin :documentation :isnew
      "Data-mixin:  
     :data-names   sets data names
     :data-values  sets data values
     :data-limits  sets data limits
     :data-integer? sets data integer constraint flag
     :data-constraint-fun sets constraint function
")

;;; :destruct --- disposes of the data
(defmeth el-data-mixin :destruct (&rest args)
  (send (slot-value 'data) :destruct)
  (setf (slot-value 'data) nil)
  (apply #'call-next-method args))


;;;--- slot setting/accessing methods ---

;;; :data-names

(defmeth el-data-mixin :data-names
  	(&optional (data-names nil set))
   (when set (send self :data :names data-names))
   (send (slot-value 'data) :names))
(send el-data-mixin :documentation :data-names
      "Returns/sets data names")

;; data methods: :data (:value :data :limits
;; :describe :names :integer? :constraint-fun)
;; signal could be :local, in which case the rest of the message is
;; interpreted as a set to the data, one of the valid
;; signals for the data or omitted in which case it defaults
;; to values
(defmeth el-data-mixin :data
  	(&optional (signal :data) &rest args)
  (when (find :data (trace)) (format t "Object: ~S~%" self))
  (when (and (listp signal) (every #'numberp (element-seq signal)))
	(push signal args) (setq signal :data))
  (if (eq signal :local)		;local set of some nature
      (progn
	(if (find :data (trace))
	    (format t ":data (el-data-mixin) send to data~%"))
	(apply #'send (cons (slot-value 'data) args)))
    (let ((set (if (member signal (send self :data-names))
		   (or (> (list-length args) 1)
		       (every #'numberp (element-seq (car args))))
		 (and (not (eql signal :describe)) args))))
      (if set
	  (apply #'send  self :update :data signal args))
      (progn
	(if (find :data (trace))
	    (format t ":data (el-data-mixin) <set>=nil send to data-object~%"))
	(apply #'send  (slot-value 'data) signal args)))))

(send el-data-mixin :documentation :data
"Usage: :data [:local] <signal> &rest <args>.
     Behavior depends on value of <signal>:
     :data, :values (or omitted signal) --- Returns/sets data values
     :limits --- Returns/sets limits
     :names --- Returns/sets names
     :integer? Returns/sets integer constraint flag
     :constraint-fun --- Returns/sets constraint function
     :describe --- sends a describe message to embeded el-data object
     <data name> ---  Returns/sets data value 
                  or with an optional qualifier Returns/sets specific
                  data information.

     For example, :data 'x --- returns data item X
        and  :data 'x :limits '(-10 10)  --- sets the limits for X values
        

If the :local flag is present, <signal> <args> is sent the
      data object.  
If not, then the method trys to establish if this is an attempt to <set>
the data.  Rule used:
     <set> --- if <signal> is a data name 
                  then <set> <-T if (length <args>) > 1 [more than just signal]
                                 or (car <args>) is a list of numbers
               else if <signal> is not :describe
                    then <set> <- T if (length <args>) >0
               else <set> <- nil
If <set> is true an appropriate :update message is generated.
Otherwise, <signal> <args> is sent to the data object.
")
  
  
(defmeth el-data-mixin :data-object (&rest args)
  (if args
      (if (eql :local (car args))
	  (setf (slot-value 'data) (cadr args))
	(apply #'send self :update :data-object args))
    (slot-value 'data)))
(send el-data-mixin :documentation :data-object
      "Returns/updates (sets with :local) actual data object")



;

;;; ------- inh-data-mixin --- Inherits data object --------


(defproto inh-data-mixin
  '(data
    ) '()
    (list inh-nui-parameter-mixin subtool-mixin ElToY-object)
"This object owns a group of prior or posterior data. It should always
be a inh-nui-parameter-mixin as well.

It has read access only to the data object.  To write the data, it
sends the write message to its parent."
)

;;; ---- Birth and Death ------

;;; :isnew --- this sets the data
(defmeth inh-data-mixin :isnew
  	(&rest args
	 &key (data nil) 
	 (parent nil) (nui-parameters nil))
  (setf (slot-value 'data) data)
  (apply #'call-next-method args))

(send inh-data-mixin :documentation :isnew
      (strcat
       "inh-data-mixin: :data <data-object> initialized object
" (send inh-nui-parameter-mixin :documentation :isnew)))

;;; :destruct --- disposes of the data
(defmeth inh-data-mixin :destruct (&rest args)
  (send (slot-value 'data) :destruct)
  (setf (slot-value 'data) nil)
  (apply #'call-next-method args))


;;;--- slot setting/accessing methods ---

;;; :data-names

(defmeth inh-data-mixin :data-names
  	(&optional (data-names nil set))
   (when set (send self :data :names data-names))
   (send (slot-value 'data) :names))
(send inh-data-mixin :documentation :data-names
      "Returns/sets data names")

;; data methods: :data (:value :data :limits
;; :describe :names :integer? :constraint-fun)
;; signal could be :local, in which case the rest of the message is
;; passed to a local handler if one exits
(defmeth inh-data-mixin :data
  	(&optional (signal :values) &rest args)
  (when (find :data (trace)) (format t "Object: ~S~%" self))
  (when (and (listp signal) (every #'numberp (element-seq signal)))
	(push signal args) (setq signal :values))
  (if (eq signal :local)		;local set of some nature
      (let* ((signal (car args))
	     (who (position signal (send self :data-names))))
	(if (send self :has-method signal)
	    (progn
	      	(if (find :data (trace))
		    (format t ":data (inh-data-mixin) dispatching to signal ~S ~%"
			    signal))
		(apply #'send (slot-value 'data) signal :local (cdr args)))
	  (when who
		(if (find :data (trace))
		    (format t ":data (inh-data-mixin) ~Sth data signal~%"
			    who))
		(apply #'send :local-data-dispatch self who (cdr args)))))
    ;; not a local call
    (let ((set (if (member signal (send self :data-names))
		   (or (> (list-length args) 1)
		       (every #'numberp (element-seq (car args))))
		 (and (not (eql signal :describe)) args))))
      (if set
	  (progn
	    	(if (find :data (trace))
		    (format t ":data (inh-data-mixin) <set>=T passing to supertool~%"))
		(apply #'send  self :supertool :data signal args))
	(progn
	  (if (find :data (trace))
	      (format t ":data (inh-data-mixin) <set>=nil handling locally~%"))
	  (apply #'send (slot-value 'data) signal args))))))


  

(send inh-data-mixin :documentation :data
"Usage: :data [:local] <signal> &rest <args>.
     Behavior depends on value of <signal>:
     :data, :values (or omitted signal) --- Returns/sets values
     :limits --- Returns/sets limits
     :names --- Returns/sets names
     :integer? Returns/sets integer constraint flag
     :constraint-fun --- Returns/sets constraint function
     :describe --- sends a describe message to embeded data object
     <data name> ---  Returns/sets data value 
                  or with an optional qualifier Returns/sets specific
                  data information.

     For example, :data 'X --- returns data value X
        and  :data 'X :limits '(-10 10)  --- sets the
        limits for X data values (updating)

If the :local flag is present, and <signal> is a valid message for the
   <self>,  object sends itself <signal> :local <args>.
If not, then the method trys to establish if this is an attempt to <set>
the data.  Rule used:
     <set> --- if <signal> is a data name 
                  then <set> <-T if (length <args>) > 1 [more than just signal]
                                 or (car <args>) is list of numbers
               else if <signal> is not :describe
                    then <set> <- T if (length <args>) >0
               else <set> <- nil
If <set> is true an appropriate :update message is generated.
Otherwise, <signal> <args> is sent to data object.
")

(defmeth inh-data-mixin :local-data-dispatch (self who &rest args)
  (let* ((signal (if (numberp (car args)) :data1
		  (car args)))
	 (sig1 (case signal
		     ((:data :value :values) :data1)
		     ((:integer :integer?) :data-integer?1)
		     ((:name :names) :data-names1)
		     (t signal))))
    (if (send self :has-method sig1)
	(apply #'send self sig1 who :local (cdr args)))))
  
;; data-object
(defmeth inh-data-mixin :data-object (&rest args)
  (if args
      (if (eql :local (car args))
	  (setf (slot-value 'data) (cadr args))
	(apply #'send self :update :data-object args))
    (slot-value 'data)))
(send inh-data-mixin :documentation :data-object
      "Returns/updates (sets with :local) actual data object")

(new-provide :el-data-mixin)
