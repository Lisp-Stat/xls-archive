;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy and use this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the distribution prior-likelihood prototypes for an
;;; elicitation tool 

(require :el-conjugate (strcat ElToY-directory "conj" *d-sep*  "conjugate.lsp"))


;;;;;;;;;;;;;;;;; mixin-methods for having a conjugate family
;;;;;;;;;;;;;;;;; subobject

;;; conjugate-family-mixin

(defproto conjugate-family-mixin
  '(conjugate-family)
  '() (list ElToY-object)
 "For adding a conjugate-family to an object"
)


;; :isnew
(defmeth conjugate-family-mixin :isnew (&rest args
					  &key (conjugate-family
						Undefined-conjugate-family)
					  &allow-other-keys)
  (setf (slot-value 'conjugate-family) conjugate-family)
  (apply #'call-next-method args))

(send conjugate-family-mixin :documentation :isnew
" :Conjugate-family --- sets conjugate family
      Note:  arg must be a family (prototype) object.
")

;; :destruct
(defmeth conjugate-family-mixin :destruct (&rest args)
  (send (slot-value 'conjugate-family) :destruct)
  (setf (slot-value 'conjugate-family) nil)
  (apply #'call-next-method args))


;;; :conjugate-family

(defmeth conjugate-family-mixin :conjugate-family (&rest args)
  (if args (apply #'send (slot-value 'conjugate-family) args)
    (slot-value 'conjugate-family)))
(send conjugate-family-mixin :documentation :conjugate-family
      "&rest args
Returns conjugate family or else sends args as message to conjugate
family. 
")

(defmeth conjugate-family-mixin :set-conjugate-family (&rest args)
  (unless (endp args)			;set parameter
	  (if (eql :local (car args))
	      (setf (slot-value 'conjugate-family) (cadr args))
	    (apply #'send self :update :set-conjugate-family (cdr args))))
  (slot-value 'conjugate-family) )
(send conjugate-family-mixin :documentation :conjugate-family
      "[:local] new-conjugate-family-object
Returns/sets the conjugate family object.  
Only sets on :local, otherwise sends :update.
")
  

;;; :prior-family & :likelihood-family    


(defmeth conjugate-family-mixin :prior-family (&rest args)
  (if args (apply #'send (send (slot-value 'conjugate-family) :prior-family)
		  args)
    (send (slot-value 'conjugate-family) :prior-family)))
(send conjugate-family-mixin :documentation :prior-family
      "&rest args
Returns prior family or else sends args as message to prior family. 
")


(defmeth conjugate-family-mixin :likelihood-family (&rest args)
  (if args (apply #'send
		  (send (slot-value 'conjugate-family) :likelihood-family)
		  args)
    (send (slot-value 'conjugate-family) :likelihood-family)))
(send conjugate-family-mixin :documentation :likelihood-family
      "&rest args
Returns likelihood family or else sends args as message to likelihood
family.  
")




;;; parameter names and data-names

(defmeth conjugate-family-mixin :parameters (&rest args)
  (if (and args (eql :local (car args)))
      (if (eql :names (cadr args))
	  (progn
	    (if (find :parameters (trace))
		(format t ":parameters (conjugate-family-mixin) send to conjugate-family~%"))
	    (send (slot-value 'conjugate-family) :parameter-names
		  (cadr args)))
	(if (and (cddr args)
		 (or (eql :names (caddr args))
		     (eql :name (caddr args))))
	    (send (slot-value 'conjugate-family) :parameter-names
		  (subst (cadr args) (cadddr args)
			 (send self :conjugate-family :parameter-names))))))
  (apply #'call-next-method args))
(send conjugate-family-mixin :documentation :parameters
      "On :local catches sets of names and matches
       conjugate-family to matchs with conjugate-family change.
")


(defmeth conjugate-family-mixin :hyperparamters (&rest args)
  (if (and args (eql :local (car args)))
      (if (eql :names (cadr args))
	  (send (slot-value 'conjugate-family) :hyperparameter-names
		(cadr args))
	(if (and (cddr args)
		 (or (eql :names (caddr args))
		     (eql :name (caddr args))))
	    (send (slot-value 'conjugate-family) :hyperparameter-names
		  (subst (cadr args) (cadddr args)
			 (send self :conjugate-family :hyperparameter-names))))))
  (apply #'call-next-method args))
(send conjugate-family-mixin :documentation :hyperparamters
      "On :local catches sets of names and matches
       conjugate-family to matchs with conjugate-family change.
")

(defmeth conjugate-family-mixin :nui-paramters (&rest args)
  (if (and args (eql :local (car args)))
      (if (eql :names (cadr args))
	  (send (slot-value 'conjugate-family) :nui-parameter-names
		(cadr args))
	(if (and (cddr args)
		 (or (eql :names (caddr args))
		     (eql :name (caddr args))))
	    (send (slot-value 'conjugate-family) :nui-parameter-names
		  (subst (cadr args) (cadddr args)
			 (send self :conjugate-family :nui-parameter-names))))))
  (apply #'call-next-method args))
(send conjugate-family-mixin :documentation :nui-paramters
      "On :local catches sets of names and matches
       conjugate-family to matchs with conjugate-family change.
")


(defmeth conjugate-family-mixin :data-names (&rest args)
  (if (eql :local (car args))
      (send (slot-value 'conjugate-family) :data-names
	    (cadr args)))
  (apply #'call-next-method args))
(send conjugate-family-mixin :documentation :data-names
      "On :local catches sets conjugate-family to match change.
")


;; :update
(defmeth conjugate-family-mixin :update (signal &rest args)
  (apply #'call-next-method signal args)
  (if (find :update (trace))
      (format t ":update (conjugate-family-mixin) Sending message to
conjugate family~%"))
  (apply #'send (slot-value 'conjugate-family) :update signal args))



;;; Link-functions

;;; :forward-link 
(defmeth conjugate-family-mixin :forward-link (&rest args)
  (apply #'send (send self :conjugate-family) :forward-link args))


;;; :reverse-link
(defmeth conjugate-family-mixin :reverse-link (&rest args)
  (apply #'send (send self :conjugate-family) :reverse-link args))
  


;


;;; inh-conjugate-family-mixin  --- inherited conjugate family from parent

(defproto inh-conjugate-family-mixin
  '(conjugate-family)
  '() (list subtool-mixin)
"For adding an inherited conjugate-family to an object"
)

(defmeth inh-conjugate-family-mixin :isnew (&rest args
				            &key conjugate-family
				            &allow-other-keys)
  (setf (slot-value 'conjugate-family) conjugate-family)
  (apply #'call-next-method args))
(send inh-conjugate-family-mixin :documentation :isnew
      ":conjugate-family --- (inherited) conjugate-family object.
")

;;; :conjugate-family

(defmeth inh-conjugate-family-mixin :conjugate-family (&optional (signal nil)
						   &rest args)
  (if (and signal (not (eql signal :local)))
      (if args (apply #'send self :supertool
		      :conjugate-family signal args)
	(apply #'send (slot-value 'conjugate-family) signal args))
    (slot-value 'conjugate-family)))
(send inh-conjugate-family-mixin :documentation :conjugate-family
      "&rest args
Returns conjugate family or else sends args as message to conjugate family. 
")



(defmeth inh-conjugate-family-mixin :set-conjugate-family (&rest args)
  (unless (endp args)			;set conjugate-family slot
	  (if (eql :local (car args)) t
	    (apply #'send self :supertool
		   :update :conjugate-family (cdr args))))
  (slot-value 'conjugate-family))
  
;;; :prior-family & :likelihood-family    



(defmeth inh-conjugate-family-mixin :prior-family (&optional (signal nil)
						   &rest args)
  (if (and signal (not (eql signal :local)))
      (if args (apply #'send self :supertool
		      :prior-family signal args)
	  (apply #'send (send (slot-value 'conjugate-family) :prior-family)
		  signal args))
    (send (slot-value 'conjugate-family) :prior-family)))
(send inh-conjugate-family-mixin :documentation :prior-family
      "&rest args
Returns prior family or else sends args as message to prior family. 
")


(defmeth inh-conjugate-family-mixin :likelihood-family (&optional (signal nil)
						   &rest args)
  (if (and signal (not (eql signal :local)))
      (if args (apply #'send :supertool
		      :likelihood-family signal args)
	  (apply #'send (send (slot-value 'conjugate-family)
			      :likelihood-family) 
		  signal args))
    (send (slot-value 'conjugate-family) :likelihood-family)))
(send inh-conjugate-family-mixin :documentation :likelihood-family
      "&rest args
Returns likelihood family or else sends args as message to likelihood
family.  
")




;; :destruct
(defmeth inh-conjugate-family-mixin :destruct (&rest args)
  (setf (slot-value 'conjugate-family) nil)
  (apply #'call-next-method args))




;;; Link-functions

;;; :forward-link 
(defmeth inh-conjugate-family-mixin :forward-link (&rest args)
  (apply #'send (send self :conjugate-family) :forward-link args))


;;; :reverse-link
(defmeth inh-conjugate-family-mixin :reverse-link (&rest args)
  (apply #'send (send self :conjugate-family) :reverse-link args))
  

(new-provide :el-conj-mixin)
