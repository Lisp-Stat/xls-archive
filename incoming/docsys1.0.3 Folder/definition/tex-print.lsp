;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: :Definitions -*-
;;; RGA modifications to JAM's definition objects
;;;  File Name          :  tex-print.lisp
;;;  Version Number     :  1.5
;;;  Last Changed Date  :  95/11/14 At 16:49:34
;;;  Created Date       :  11/14/95

(in-package :Definitions)

(defparameter *definition-hfuzz* 5
  "Amount of slop, or Hfuzz (in points), in TeX line breaking
  algorithm.  Increasting this value will encourage TeX to stop
  complaining about overfull hboxes.") 

(unless (fboundp 'declaim)
  
(defmacro declaim (&rest decl-specs)
  (let ((forms nil))
    (dolist (decl-spec decl-specs)
      (push `(proclaim ',decl-spec) forms))
    `(progn ,@forms)))
  
)

(declaim (type Fixnum *definition-hfuzz*))


;;;-------------------------------------------------------

;;;(defmethod print-tex-object ((def Definition) stream)

(defmeth Definition :print (&optional (stream t))
  "A generic method for printing Definition objects."
  (declare  (type (or Stream T Nil) stream)
	   (:returns def))
  (format stream "#<Def: ~a ~s>"
	  (definition-class-nice-name self)
	  (definition-name self))
  self)

;;;=======================================================


(defmacro with-readable-printing (&rest forms)
  "Creates a readable style for most output variables.
   Recompile file if this macro is changed."
  `(let ((*print-case* :downcase)
	#+:excl (excl:*print-nickname* nil)
	 (*print-right-margin* 132))
     ,.forms))

;; leave this alone for now, may come back to it.

(defun fix-TeX-string (s0)
  
  "Massage a string so that TeX special characters will come out
as something reasonable.
See \cite{Lamp86} pp. 15, 65."

  (declare (type String s0)
	   (:returns (type String)))
  (with-output-to-string (out)
    (dotimes (i (length s0))
      (let ((c (char s0 i)))
	(case c
	  ((#\# #\$ #\% #\& #\_ #\{ #\} #\> #\< #\^ #\~ #\\)
	   ;; all these need is a \
	   (write-char #\\ out)
	   (write-char c out))
	  (( )
	   ;; these need to be in verbatim environment
	   (format out "\\verb.")
	   (write-char c out)
	   (write-char #\. out)
	   )
	  ((#\|)
	   ;; force these into a \tt font,
	   ;; where they print as the should
	   (format out "{\\tt")
	   (write-char c out)
	   (write-char #\} out)
	   )
	  (otherwise
	   (write-char c out)))))))

;;;=======================================================

(defgeneric print-tex-definition-headline (def stream)
  (:documentation   "Print a Headline for the definition.")
  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))
  )



(defmethod print-tex-definition-headline ((def Definition) 
				      &key
				      (stream *standard-output*))

  "Print a Headline for the definition."

  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))

  (with-readable-printing
      (let ((name-string (fix-TeX-string (definition-name->string def))))
	
    ;; name box
	(format stream "~%~%\\DefNameBox{~A}{~A}~%"
		name-string (definition-class-nice-name def))
	def)))

;;;=======================================================
(defgeneric print-tex-definition-usage (def &key stream)
  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))
  (:documentation "Print a description of how to ``call'' the definition.")
  )


(defmethod print-tex-definition-usage ((def Definition) 
				   &key
				   (stream *standard-output*))

  "Print a description of how to ``call'' the definition."

  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))

  (with-readable-printing
      (multiple-value-bind (usage-string usage-indent)
	  (definition-usage def)
	(format stream "\\Usage{~D}{\\cd{~A}}\\endUsage~%"
	    usage-indent usage-string))
    def))

;;;=======================================================

(defgeneric print-tex-definition-arg-types (def &key stream)
  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))
  (:documentation "Print a list of args and their expected types.")
  )


(defmethod print-tex-definition-arg-types ((def Definition) 
				       &key
				       (stream *standard-output*))

  "Print a list of args and their expected types."

  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))
  (with-readable-printing
      (let ((arg-types (definition-arg-types def)))
	(when arg-types
	  (format stream "\\beginArguments~%")
	  (dolist (arg-type arg-types)
	    (format stream
		    "\\argument{\\cd{~A}}\\typeArg{\\cd{~:(~S~)}}\\endArg~%" 
		    (first arg-type)
		    (second arg-type)))
	  (format stream "\\endArguments~%"))))
  def)
;;;=======================================================
(defgeneric print-tex-definition-slot-types (def &key stream)
  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))
	(:documentation 
	"Print a list of visible slot and their expected types and 
	documentation.")
  )

(defmethod print-tex-definition-slot-types ((def Definition) 
				       &key
				       (stream *standard-output*))

  "Print a list of visible slots and their expected types and documentation."
  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))
  def)


(defmethod print-tex-definition-slot-types ((def Class-Definition)
					   &key (stream *standard-output*))
  (with-readable-printing
      (let ((slot-specs (definition-slots def)))
	(when slot-specs
	  (format stream "\\beginSlots~%")
	  (dolist (slot-spec slot-specs)
	    (let ((name (car slot-spec))
		  (initargs (get-slot-initarg-list slot-spec))
		  (slot-type (getf (cdr slot-spec) :type T))
		  (slot-doc (getf (cdr slot-spec) :documentation)))
	      (when initargs
		(format stream
			"\\slot{\\cd{~A}}\\initargsSlot\\cd{~:(~S~)}\\typeSlot{\\cd{~:(~S~)}}"
			name initargs slot-type)
		(if slot-doc
		    (format stream "\\docSlot{~A}\\endSlot~%"
			    slot-doc)
		  (format stream "\\endSlot~%")))))
	  (format stream "\\endSlots~%"))))
  def)


;;;=======================================================

(defun tex-format-return-spec (return-spec stream)
  (cond ((atom return-spec)
	 (format stream "\\simpleReturn{\\cd{~A}}\\endsReturn~%" return-spec))
	((eq (first return-spec) 'type)
	 (ecase (length return-spec)
	   (2 (format stream "\\typeReturn{\\cd{~:(~S~)}}\\endtReturn~%"
		      (second return-spec)))
	   (3 (format stream "\\complexReturn{\\cd{~a}}\\midReturn{\\cd{~:(~s~)}}\\endcReturn~%"
		      (third return-spec) (second return-spec)))))
	(t (error "Invalid :returns spec"))))


(defgeneric print-tex-definition-returns (def &key stream)
  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))
  (:documentation "Print a list of returned values and/or their
      (let ((children (types.")
  )

(defmethod print-tex-definition-returns ((def Definition) 
				       &key
				       (stream *standard-output*))

  "Print a list of returned values and/or their types."

  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))

  (with-readable-printing
      (let ((returns (definition-returns def)))
	(when returns
	  (format stream "\\beginReturn~%")
	  (cond ((and (listp (second returns))
		      (eq (first (second returns)) 'values))
		 (dolist (return-spec (rest (second returns)))
		   (format stream "\\itemReturn ")
		   (tex-format-return-spec return-spec stream)))
		(t ;; else a single return value
		 (format stream "\\singleReturn ")
		 (tex-format-return-spec (second returns) stream)))
	  (format stream "\\endReturn~%"))))
  def)


;;;=======================================================


(defgeneric print-tex-definition-parents (def &key stream)
  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))
  (:documentation "Print a list of parent definitions.")
  )


(defmethod print-tex-definition-parents ((def Definition) 
				       &key
				       (stream *standard-output*))

  "Print a list of parent definitions."

  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))

  (with-readable-printing
      (let ((parents (mapcar #'princ-with-nicknames (definition-parents def))))

	(when parents
	  (format stream "~%\\Parents{~A}\\endParent~%"
		  (format nil "~{\\nextInList ~:(~A~)~}"
			  (mapcar #'fix-tex-string parents))))))
  def)


;;;=======================================================


(defgeneric print-tex-definition-children (def &key stream)
  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))
  (:documentation "Print a list of objects derived from this object.")
  )


(defmethod print-tex-definition-children ((def Definition) 
				       &key
				       (stream *standard-output*))

  "Print a list of derived objects."

  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))

  (with-readable-printing
      (let ((children (mapcar #'princ-with-nicknames (definition-children def))))
	
	(when children
	  (format stream "\\Children{~A}\\endChildren~%"
		  ;; Added newline at the begining of each one in case
		  ;; there are too many methods
		  (format nil "~{\\nextInList~% ~:(~A~)~}"
			  (mapcar #'fix-tex-string children))))))
  def)

;;;=======================================================

(defgeneric print-tex-definition-documentation (def &key stream)
 (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))
 (:documentation "Print the documentation string for the definition.")
 )


(defmethod print-tex-definition-documentation ((def Definition) 
					   &key
					   (stream *standard-output*))

  "Print the documentation string for the definition."

  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))

  (with-readable-printing
      (let ((documentation (fix-TeX-string (definition-documentation def))))
	(when (and documentation (not (zerop (length documentation))))
	  (format stream "\\beginDocumentation~%")
	  (format stream "~A" documentation)
	  (format stream "\\endDocumentation~%"))))
  def)

;;;=======================================================

(defgeneric print-tex-definition-source-path (def &key stream)
  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))
  (:documentation "Print the pathname from which this definition was
      (let ((children (read.")
  )


(defmethod print-tex-definition-source-path ((def Definition) 
					 &key
					 (stream *standard-output*))

  "Print the pathname from which this definition was read."

  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))

  (with-readable-printing
      (let ((path (definition-path def)))
	(when path
	  (format stream "~%\\Source{ ~A}~%"
		  (fix-TeX-string (format nil "~a" path))))))
  def)


;;;=======================================================

(defgeneric print-tex-definition-methods (def &key stream all)
  (:documentation "Prints out a list of methods associated with this definition.")
  )

(defmethod print-tex-definition-methods ((def Definition) &key stream all)
  "Prints out a list of methods associated with this definition."

  (declare (type Definition def)
	   (type (or T Null) all)
	   (type Stream stream)
	   (:returns def))

  (with-readable-printing
      (let ((methods (mapcar #'princ-with-nicknames
			     (definition-methods def :all all)))) 
	
	(when methods
	  (format stream "\\beginMethods{~A}\\endMethods~%"
		  ;; Added newline at the begining of each one in case
		  ;; there are too many methods
		  (format nil "~{\\nextInList~% ~:(~A~)~}"
			  (mapcar #'fix-tex-string methods))))))
  def)


(defgeneric print-tex-definition-signals (def &key stream)
  (:documentataion "Prints out a list of announcements associated with this definition.")
  )

(defmethod print-tex-definition-signals ((def Definition) &key stream)
  "Prints out a list of announcements associated with this definition."
  def)


;;;----------------------------------------------------------------------
(defgeneric print-tex-definition (def &key stream)
	    (declare (type Definition def)
		     (type Stream stream))
	    (:documentation
	     "Print a definition object in TeX."))

(defmethod print-tex-definition ((def Definition) 
			     &key
			     (stream *standard-output*))

  "Prints out the reference manual entry for a definition object in TeX."

  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))

  (with-readable-printing
      (let (
	    ;;(name-string (fix-TeX-string (definition-name->string def)))
	    )

	(format stream "~%\\beginDefinition~%")
	(print-tex-definition-headline def :stream stream)
	(print-tex-definition-documentation def :stream stream)
	(print-tex-definition-usage def :stream stream)
	(print-tex-definition-arg-types def :stream stream)
	(print-tex-definition-slot-types def :stream stream)
	(print-tex-definition-returns def :stream stream)
	(print-tex-definition-parents def :stream stream)
	(print-tex-definition-children def :stream stream)
	(print-tex-definition-methods def :stream stream)
	(print-tex-definition-signals def :stream stream)
	(print-tex-definition-source-path def :stream stream)
	(format stream "\\endDefinition~%~%")
	;;(format stream "~%\\markboth{~a}{~a}" name-string name-string)
	def)))

;;;=======================================================

(defun print-tex-definitions (defs path &key (mode :tex))

  "Print a TeX representation of the definition objects in <defs>
on the file corresponding to <path>.  <Mode> should be either :tex for
Plain TeX or :latex for LaTeX."

  (declare (type List defs)
	   (type (or String Pathname) path)
	   (type (member '(:Tex :Latex)) mode)
	   (:returns defs))

  (with-open-file (s path
		   :direction :output
		   :if-does-not-exist :create
		   :if-exists :supersede)
    (format s "\\input ~A~%\\hfuzz=~Dpt~%~%"
	    (if (eql mode :tex)
		user::*definition-tex-header-file*
	      user::*definition-latex-header-file*)
	    *definition-hfuzz*)
    (dolist (def defs) (print-tex-definition def :stream s)))

  (values defs))




(defmethod print-tex-definition-slot-types ((def Prototype-Definition)
					   &key (stream *standard-output*))
  (let ((definee (definition-definee def)))
    (with-readable-printing
	(let ((slots (send definee :own-slots)))
	  (when slots
	    (format stream "\\beginSlots~%")
	    (dolist (slot slots)
	      (let ((slot-type (send definee :slot-type slot))
		    (slot-doc (send definee :documentation slot)))
		(format stream
			"\\KRSlot{\\cd{~(~S~)}}\\typeKRSlot{\\cd{~:(~S~)}}"
			slot slot-type)
		(if slot-doc
		    (format stream "\\docKRSlot{~A}\\endKRSlot~%"
			    slot-doc)
		  (format stream "\\endKRSlot~%"))))
	    (format stream "\\endSlots~%")))))
  def)


