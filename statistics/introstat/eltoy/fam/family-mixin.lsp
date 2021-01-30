;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy and use this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the distribution family prototypes for an elicitation tool

(require :el-family (strcat ElToY-directory "fam" *d-sep*  "family.lsp"))

;;;----------------------------------------------------------------------
;;; family-mixin 
;;;----------------------------------------------------------------------

;;; includes a family slot of an object.

(defproto family-mixin
  '(family)
  '() (list ElToY-object)
 "For adding a family to an object"
)


;; :isnew
(defmeth family-mixin :isnew (&rest args
				      &key (family Undefined-family)
				      &allow-other-keys)
  (setf (slot-value 'family) family)
  (apply #'call-next-method args))

(send family-mixin :documentation :isnew
" :family --- sets family
      Note:  arg must be a family (prototype) object.
")

;; :destruct
(defmeth family-mixin :destruct (&rest args)
  (send (slot-value 'family) :destruct)
  (setf (slot-value 'family) nil)
  (apply #'call-next-method args))

;;; :family

(defmeth family-mixin :family (&rest args)
  (if args (apply #'send (slot-value 'family) args)
    (slot-value 'family)))
(send family-mixin :documentation :family
      "&rest args
Returns family or else sends args as message to family. 
")


(defmeth family-mixin :set-family (&rest args)
  (unless (endp args)			;set parameter
	  (if (eql :local (car args))
	      (setf (slot-value 'family) (cadr args))
	    (apply #'send self :update :set-family (cdr args))))
  (slot-value 'family) )
(send family-mixin :documentation :family
      "[:local] new-family-object
Returns/sets the family object.  
Only sets on :local, otherwise sends :update.
")
  
(defmeth family-mixin :parameter-names (&rest args)
  (if (eql :local (car args))
      (send (slot-value 'family) :parameter-names
	    (cadr args)))
  (apply #'call-next-method args))
(send family-mixin :documentation :parameter-names
      "On :local catches sets family to match change.
")

(defmeth family-mixin :rv-names (&rest args)
  (if (eql :local (car args))
      (send self :family :rv-names (cadr args))
    (if args (apply #'send self :update :rv-names args)
      (send self :family :rv-names))))
(send family-mixin :documentation :rv-names
      "Returns/sets rv-names (taken from family)
")


;; :update
(defmeth family-mixin :update (signal &rest args)
  (apply #'call-next-method signal args)
  (if (find :update (trace))
      (format t ":update (family-mixin) send to family~%"))
  (apply #'send (slot-value 'family) :update signal args))


;
;;;----------------------------------------------------------------------
;;; inh-family-mixin
;;;----------------------------------------------------------------------

;;; mixes in inherited family object slot


;;; inh-family-mixin  --- inherited conjugate family from parent

(defproto inh-family-mixin
  '(family)
  '() (list subtool-mixin)
"For adding an inherited family to an object"
)

(defmeth inh-family-mixin :isnew (&rest args
				            &key family
				            &allow-other-keys)
  (setf (slot-value 'family) family)
  (apply #'call-next-method args))
(send inh-family-mixin :documentation :isnew
      ":family --- (inherited) family object.
")

;;; :family

(defmeth inh-family-mixin :family (&optional (signal nil)
						   &rest args)
  (case signal
      (nil (slot-value 'family))
      ((:parameter-integer? :parameter-granularity
	:parameter-range-default :parameter-limits
	:default-parameters :parameter-constraint-fun
	:parameter-names :rv-names)
       (if args (apply #'send self :supertool
		       :family signal args)
	 (apply #'send (slot-value 'family) signal args)))
      (t (apply #'send (slot-value 'family) signal args))))
(send inh-family-mixin :documentation :family
      "&rest args
Returns conjugate family or else sends args as message to conjugate family. 
")



(defmeth inh-family-mixin :set-family (&rest args)
  (unless (endp args)			;set parameter
	  (if (eql :local (car args)) t
	    (apply #'send self :supertool
		   :update :family (cdr args))))
  (slot-value 'family))
  

;; :destruct
(defmeth inh-family-mixin :destruct (&rest args)
  (setf (slot-value 'family) nil)
  (apply #'call-next-method args))



(defmeth inh-family-mixin :rv-names (&rest args)
  (if (eql :local (car args)) t
    (if args (apply #'send self :supertool :update :rv-names args)
      (send self :family :rv-names))))
(send inh-family-mixin :documentation :rv-names
      "Returns/updates rv-names (taken from family) 
")

(new-provide :el-family-mixin)
