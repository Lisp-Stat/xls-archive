;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the objects for data manipulation


(require :el-nui-parameters (strcat ElToY-directory "params" *d-sep*  "nui-params.lsp"))
(require :el-constraint (strcat ElToY-directory "utils" *d-sep*  "constraint.lsp"))
(require :el-utils (strcat ElToY-directory "utils" *d-sep*  "utils.lsp"))
(require :el-checks (strcat ElToY-directory "utils" *d-sep*  "checks.lsp"))

;;; el-data is a primitive data object.  It keeps a pointer to the set
;;; of nuisance parameters which might constrain its values.

(defproto el-data-proto
  '(names
    data
    limits
    integer?
    ndata
    constraint-fun
    ) '()
    (list inh-nui-parameter-mixin ElToY-object)
"This object creates a group of data; possibly vectorized."
)

;;; --- utility methods:  :print and :describe ---

; :print method
(defmeth el-data-proto :print (&optional (stream t))
   (format stream "#<el-data: (~S)>"
	   (send self :names)))


; :describe method --- verbose argument controls amount of excess
; printing done (t or integer) controls printing.
(defmeth el-data-proto :describe (&optional (stream t) (verbose 1))
   (format stream "El Data: ~S~%"
	   (send self :names))
   (format stream "Values are: ~S~%"
	   (send self :data))
   (when (or (eq verbose t)
	     (and (numberp verbose) (>= verbose 1)))
	 (format stream "Integer values? ~S~%"
		 (send self :integer?))
	 (when (or (eq verbose t)
		   (and (numberp verbose) (>= verbose 2)))
	       (format stream "Hard limits of values ~S~%"
		       (send self :limits)))
	 (call-next-method stream verbose)))


;;; ---- Birth and Death: :isnew and :destruct ---

(defmeth el-data-proto :isnew
  (&rest args
   &key (data nil) (names nil)
        (limits nil) (integer? nil)
	(constraint-fun vacuous-data-constraint)
	(nui-parameters nil)
   &allow-other-keys)
  
  ;; first set up nuisance parameters
  (apply #'call-next-method :nui-parameters nui-parameters args)

  ;; check for equal list lengths
  (unless (listp integer?) (setq integer? (list integer?)))
  (setf (slot-value 'ndata)
	(max (list-length data) (list-length names)
	     (list-length limits) (list-length integer?)))
  (when (null data) (setq data (repeat '(nil) (slot-value 'ndata))))
  ;; integer?
  (if (null integer?) (setq integer? (list nil)))
  (if (eql 1 (list-length integer?))
      (setq integer? (repeat integer? (slot-value 'ndata))))
  (unless (eql (slot-value 'ndata) (list-length integer?))
	  (error "el-data::isnew --- bad integer?: ~S~%" integer?))
  ;; data
  (unless (or (null data)
	      (and (eql (slot-value 'ndata) (list-length data))
		   (every #'checknump data integer?)))
	  (error "el-data::isnew --- bad data: ~S (~S ~S) [(or ~S (and
~S ~S))] ~S" data
		 integer? (slot-value 'ndata) (null data)
		 (eql (slot-value 'ndata) (list-length data))
		 (every #'checknump data integer?)
		 (or (null data)
		     (and (eql (slot-value 'ndata) (list-length data))
			  (every #'checknump data integer?)))))
  (unless (or (null names) (eql (slot-value 'ndata) (list-length names)))
	  (error "el-data::isnew --- bad names: ~S" names))
  ;;limits
  (if (null limits) (setq limits (make-list (slot-value 'ndata)
					  :initial-element *default-limits*))
    (unless (and (eql (slot-value 'ndata) (list-length limits))
		 (every #'test-pair limits integer?))
	    (error "el-data::isnew --- bad limit: ~S" limits)))
  (unless (or (null data) (every #'between data limits))
	  (error "el-data::isnew --- data not between limits: ~S ~S"
		 data limits))
  ;; check that constraint-function is satisfied
  (unless (funcall constraint-fun data (send self :nui-parameters) :warn t)
	  (error "el-data::isnew --- Unconstrained data: ~S"
		 data))
    
					;we're ok, 
					;set local parameters
  (send self :names names)
  (setf (slot-value 'integer?) integer?)
  (setf (slot-value 'data) data)
  (setf (slot-value 'limits) limits)
  (setf (slot-value 'constraint-fun) constraint-fun)
  )

(send el-data-proto :documentation :isnew
(strcat      
"El-Data-proto:  :names --- sets names (list of symbols)
                 :data --- sets values (list of numbers)
                 :limits --- sets limits (list of number pairs)
                 :integer? --- are data integer? (list of (or T nil))
	         :constraint-fun --- constraint function for data
"
(send inh-nui-parameter-mixin :documentation :isnew)))

;; destruct
(defmeth el-data-proto :destruct (&rest args)
  (setf (slot-value 'data) nil)
  (setf (slot-value 'names) nil)
  (setf (slot-value 'limits) nil)
  (setf (slot-value 'integer?) nil)
  (setf (slot-value 'constraint-fun) nil)
  (setf (slot-value 'ndata) nil)
  (apply #'call-next-method args))


;;;------ various generic set/return methods --------

;; data
(defmeth el-data-proto :data (&optional (data nil set))
   (when set 
	 (unless (and (eql (slot-value 'ndata) (list-length data))
		      (every #'numberp (element-seq data )))
		 (error "el-data::data --- bad data: ~S" data))
	 (setq data (round-if data (slot-value 'integer?)))
	 (unless (every #'between data (slot-value 'limits))
		 (uerror "Bring within limits "
			 "el-data::data --- data outside hard limits: ~S ~S"
			 data (slot-value 'limits)))
	 ;; check that constraint-function is satisfied
	 (unless (funcall (send self :constraint-fun) data
			  (send self :nui-parameters) :warn t)
		 (error "el-data::data --- Unconstrained data: ~S"
			data))
	 (setf (slot-value 'data) data))
   (slot-value 'data))

(send el-data-proto :documentation :data
"Method args: (&optional value-list)
Retrieves/sets list of data (real numbers between limits).

If data not between limits, error signaled.
If value is constrained to be an integer, it is rounded to the nearest
integer. 
")
(defmeth el-data-proto :values (&rest args)
  (apply #'send self :data args))
(send el-data-proto :documentation :values
      (send el-data-proto :documentation :data))

(defmeth el-data-proto :data1 (who &optional (data nil set))
   (when set				;setting data
	 (unless (checknump data nil)
		 (error "el-data::data1 --- bad value: ~S" data))
	 (if (nth who (slot-value 'integer?))
	     (setq data (round data)))
	 (unless (between data (nth who (slot-value 'limits)))
		 (uerror "Bring within limits "
			 "el-data::data1 --- value outside hard limits: ~S ~S"
			 data (nth who (slot-value 'limits))))
	 ;; check that constraint-function is satisfied
	 (let ((all-data (copy-list (send self :data))))
	   (setf (nth who all-data) data)
	   (unless (funcall (send self :constraint-fun) all-data
			    (send self :nui-parameters) :warn t)
		   (error "el-data::data1 --- Unconstrained data: ~S"
			all-data)))
	 
	 (setf (nth who (slot-value 'data)) data))
   (nth who (slot-value 'data)))

(send el-data-proto :documentation :data1
"Method args: (who &optional new-value)
               who is integer
Retrieves/sets nth value in list (real numbers between limits).

If new-value not between limits, error signaled.
If new-value is constrained to be an integer, it is rounded.
")
(defmeth el-data-proto :values1 (&rest args)
  (apply #'send self :data1 args))
(send el-data-proto :documentation :values1
      (send el-data-proto :documentation :data1))

;; limits
(defmeth el-data-proto :limits (&optional (limits nil set))
   (when set 
	 (unless (and (eql (slot-value 'ndata) (list-length limits))
		      (every #'test-pair limits (slot-value 'integer?)))
		 (error "el-data::limits --- bad limits: ~S" limits))
	 (setf (slot-value 'limits) limits)
 	 (unless (every #'between (slot-value 'data) limits)
		 (send self :data
		       (mapcar #'force-between (slot-value 'data) limits))))
   (slot-value 'limits))

(send el-data-proto :documentation :limits
"Method args: (&optional limits-list)
Retrieves/sets list of limits (list of pairs of numbers).

If data not within limits, data are forced between.
If data constrained to be integer, limit data must be integers.
")


(defmeth el-data-proto :limits1 (who &optional (limits nil set))
   (when set 
	 (unless (test-pair limits (nth who (slot-value 'integer?)))
		 (error "el-data::limits1 --- bad limits: ~S" limits))
	 (setf (nth who (slot-value 'limits)) limits)
 	 (unless (between (nth who (slot-value 'data)) limits)
		 (send self :range1 who
		       (force-between (nth who (slot-value 'data)) limits))))
   (nth who (slot-value 'limits)))

(send el-data-proto :documentation :limits1
"Method args: (who &optional limits-list)
               who --- integer
Retrieves/sets  whoth limits (pair of numbers).

If data not within limits, data are forced between.
If parameter constrained to be integer, limit data must be integers.
")

;; integer?
(defmeth el-data-proto :integer? (&optional (integer? nil set))
   (when set
	 (unless (eql (list-length integer?) (slot-value 'ndata))
		 (error "para:integer? Bad integer? ~S" integer?))
	 (setf (slot-value 'integer?) integer?)
	 (send self :limits (round-if (slot-value 'limits) integer?))
	 (send self :data (round-if (slot-value 'data) integer?))
	 )
   (slot-value 'integer?))

(send el-data-proto :documentation :integer?
  "Returns/ sets integer constraint.  On set, forces
limits and data to be integers.
")


(defmeth el-data-proto :integer?1 (who &optional (integer? nil set))
   (when set
	 (setf (nth who (slot-value 'integer?)) integer?)
	 (if integer?
	     (send self :limits1 who
		   (round-if (nth who (slot-value 'limits)) integer?))
	 (send self :data1 who
	       (round-if (nth who (slot-value 'data)) integer?))
	 ))
   (nth who (slot-value 'integer?)))

(send el-data-proto :documentation :integer?1
  "Returns/ sets integer constraint.  On set, forces
limits and data to be integers.
")

;;;; ----- Name set/return methods
;; :names  changing the names is a little bit different because we
;; must create new methods which go with the appropriate name
(defmeth el-data-proto :names (&optional (names nil set))
   (when set
	 (unless (eql (list-length names) (slot-value 'ndata))
		 (error "para:names Bad names ~S" names))
	 (mapc #'(lambda (old-name)
		   (send self :delete-method old-name)
		   (send self :delete-documentation old-name))
	       (slot-value 'names))
	 (setf (slot-value 'names) names)
	 (mapc #'(lambda (name)
		   (send self :add-method name
			 #'(lambda (self &rest args)
			     (apply #'el-data-proto-el-data-method
				     self (position name names) args)))
		   (send self :documentation name
			 el-data-proto-el-data-method-doc))
	       names))
   (slot-value 'names))

(send el-data-proto :documentation :names
"Method args: (&optional new-name-list)
Retrieves/sets name list

Also defines method for parameter object for each name <new-name> which
returns/sets parameter data (default), integer? and limits.
")




(defmeth el-data-proto :names1 (who &optional (new-name nil set))
  (when set
	  (send self :delete-method (nth who (slot-value 'names)))
	  (send self :delete-documentation (nth who (slot-value 'names)))
	  (setf (nth who (slot-value 'names)) new-name)
	  (send self :add-method new-name
		#'(lambda (self &rest args)
		    (apply #'el-data-proto-el-data-method
			    self (position name names) args)))
	  (send self :documentation new-name el-data-proto-el-data-method-doc))
  (nth who (slot-value 'names)))

(send el-data-proto :documentation :names1
"Method args: (who &optional new-name)
               who --- integer
Retrieves/sets  whoth name

Also defines method for parameter object with name <new-name> which
returns/sets parameter data (default), limits, and integer?.
")



;;; generic name handler.  For each parameter name, the el-data-proto
;;; assigns a method.  The method takes several signals:  

(defvar el-data-proto-el-data-method-doc 
" With no argument, this method retuns the named parameter data
  With a numeric arugment, this method sets the named parameter data
  With signal arguments:
     :data (or omited/numeric data) returns/sets the data
     :limit, :limits --- returns/changes the limits
     :integer? --- returns/changes the force intger flag
     :name, :names --- returns/changes the name
")

(defun el-data-proto-el-data-method (self who &rest args &aux (signal :data))
  (unless (or (null args) (numberp (car args)))
	  (setq signal (pop args)))
  (case signal
     (:data (apply #'send  self :data1 who args))
     ((:limit :limits) (apply #'send  self :limits1 who args))
     ((:integer :integer?) (apply #'send  self :integer?1 who args))
     ((:name :names) (apply #'send  self :names1 who args))
     (t (error "el-data::bad parameter message: ~S" args))))

;
;;; Constraint-functions

(defmeth el-data-proto :constraint-fun (&optional (new-fun nil set))
   (when set
	 (unless (functionp new-fun)
		 (error "el-data::constraint-fun Bad function ~S" new-fun))
	 (setf (slot-value 'constraint-fun) new-fun))
   (slot-value 'constraint-fun))

(send el-data-proto :documentation :constraint-fun
      "Returns/Sets additional constraint function for data.

Constraint functions should take a list of data and a list of nuisance
parameters plus an optional :warn keyword arg (default t).  They may
take additional keyword args.  They should return t if the constraints
among the data and parameters are satisfied and nil otherwise.  If
:warn is t, they should print a warning message describing the
error.")



(new-provide :el-data)

 
