;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: :Definitions -*-
;;;  File Name          :  schema.lisp
;;;  Version Number     :  1.2
;;;  Last Changed Date  :  93/08/20 At 19:55:07
;;;  Created Date       :  8/20/93
;;;
;;; Copyright 1998. Russell G. Almond. Based on work of John A.
;;; McDonald  
;;;
;;; This code may be distributed under the same terms as the rest of
;;; the arrizona package, specifically:
;;;
;;; Use and copying of this software eparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and no warranty---about the
;;; software, its performance, or its conformity to any
;;; specification---is given or implied.
;;; 
;;;
;;; 8/19/93 RGA Added announcement to list of defs.
;;;=======================================================

(in-package :definitions)

(defclass Prototype-Definition (User-Type-Definition) ()
  (:documentation
   "A definition class for defproto"))

(defmethod definition-definee ((def Prototype-Definition))
  "Get the corresponding prototype object."
  (declare (type Prototype-Definition def)
	   (:returns (type Object)))
  (symbol-value (definition-symbol def)))


(defmethod definition-documentation ((def Prototype-Definition))
  "Return the class documentation string or an empty string."
  (declare (type Prototype-Definition def)
	   (:returns (type String)))
  (let ((doc
	 (send (definition-definee def) :documentation 'proto)))
    (if (stringp doc) doc
      "")))

(defmethod definition-slots ((def Prototype-Definition))
  "Return the forms defining the visible slots of this class."
  (declare (type Prototype-Definition def)
	   (:returns (type List)))
  (let ((definee (definition-definee def)))
    (send definee :own-slots)))

(defmethod definition-methods ((def Prototype-Definition) &key all)
  "The default method returns ()."
  (declare (type Definition def)
	   (type (or T Nil) all)
	   (:returns ()))
  (let ((obj (definition-definee def)))
    (if all
	(send obj :method-selectors)
      (send obj :own-methods))))

(defmethod definition-parents ((def Prototype-Definition))
  "Returns the names of the direct superclasses."
  (declare (type Prototype-Definition def)
	   (:returns (type List)))
  (map 'List #'prototype-name
       (send (definition-definee def) :parents)))

(defun prototype-name (proto)
  "This utility function returns the name of the prototype of the
  object.  It is useful for printing parent lists."
  (send proto :slot-value 'proto-name))

(defmethod definition-children ((def Prototype-Definition) &optional package)

  "Returns the names of the direct subclasses. This method requires
the class definition to be loaded and returns the names of all direct
subclasses, not just those that have corresponding definition
objects."

  (declare (type Prototype-Definition def)
	   (:returns (type List)))
  (let ((result nil) obj
	(definee (definition-definee def))
	)
    (if package
	(do-external-symbols (sym package)
	  (when (and (boundp sym) (objectp (symbol-value sym)))
	    (setq obj (symbol-value sym))
	    (if (and (kind-of-p obj definee)
		     (not (eql obj definee)))
		(push (prototype-name obj) result))))
      (do-all-symbols (sym)
	(when (and (boundp sym) (objectp (symbol-value sym)))
	  (setq obj (symbol-value sym))
	  (if (and (kind-of-p obj definee)
		   (not (eql obj definee)))
	      (push (prototype-name obj) result))))
      )
    result
    ))

	    



(defmethod definition-usage ((def Prototype-Definition))
  (declare (type Definition def)
	   (:returns (values (type String usage)
			     (type Fixnum indent))))
  (values
   (format nil "(send ~a :new ...)"
	   (definition-name->string def))
   0))



(setf (definer-class 'defproto) 'Prototype-Definition)


(defmethod definition-class-nice-name ((def Prototype-Definition))
  (declare (:returns "Prototype"))
  "Prototype")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Messages.

(defclass Message-Definition (Method-Definition) ()
  (:documentation
   "A definition class for defmeth.  Note call Message to avoid name
  class with CLOS method."))

(defmethod definition-body ((def Message-Definition))
  "Default method extracts (nthcdr 3)"
  ;; Most defs have (defmeth obj name args . body)
  (nthcdr 4 (definition-form def)))

(defmethod definition-usage ((def Message-Definition))
  "Construct a string reflecting a typical send call."
  (declare (type Definition def)
	   (:returns (values (type String usage)
			     (type Fixnum indent))))
  (let* ((*print-case* :downcase)
	 #+:excl(excl:*print-nickname* t)
	 (name (definition-name def))
	 (lambda-list (definition-lambda-list def))
	 (rest-arg-name (lambda-list-rest-arg-name lambda-list))
	 (required-arg-names (lambda-list-required-arg-names lambda-list))
	 (optional-arg-names (lambda-list-optional-arg-names lambda-list))
	 (keyword-arg-names (lambda-list-keyword-arg-names lambda-list)))
    (when (and (listp name) (eq (first name) :message))
      (setf name (second name)))
    (cond ((atom name)
	   (values
	    (format nil
	     "(send <obj> ~A~{ ~a~}~@[ &optional~{ ~a~}~]~@[ &rest ~a~]~@[ &key~{ ~a~}~])"
	     (princ-with-nicknames name) 
	     required-arg-names optional-arg-names
	     rest-arg-name keyword-arg-names)
	    (length (format nil "(~A " (princ-with-nicknames name)))))
	  (t
	   (error "Don't know how to make a usage string for ~a" def)))
    ))

(defmethod definition-class-nice-name ((def Message-Definition))
  (declare (:returns "Message"))
  "Message")

(defmethod definition-name ((def Message-Definition))

  "The <definition-name> of a method is a list whose first item is the
symbol :message, whose second item is the function name, and whose
remaining items are the specializers for the required arguments."

  (declare (type Definition def)
	   (:returns (type List)))

  `(:message ,(third (definition-form def))
	     ,@(definition-specializers def)))


(defmethod definition-lambda-list ((def Message-Definition))
  "Finding a method's lambda list."
  (declare (type Message-Definition def)
	   (:returns (type List)))
  (fourth (definition-form def)))

(defmethod definition-name->string ((def Message-Definition))

  "The name string for methods includes the specializers, so the
different methods for a generic function can be distinguished."
  (declare (type Message-Definition def)
	   (:returns (type String)))
  (let ((*print-case* :downcase)
	#+:excl (excl:*print-nickname* t))
    (format nil "~A -> ~A"
	    (princ-with-nicknames (definition-symbol def))
	    (mapcar #'princ-with-nicknames (definition-specializers def)))))

(defmethod definition-name->tag ((def Message-Definition))

  "The name string for methods includes the specializers, so the
different methods for a generic function can be distinguished."
  (declare (type Method-Definition def)
	   (:returns (type String)))
  (substitute #\- #\?			;#\? looses in tag
	      (format nil "Method|~A~{|~A~}"
		      (princ-with-nicknames (definition-symbol def))
		      (mapcar #'princ-with-nicknames 
			      (definition-specializers def))
		      )))

(defmethod definition-specializers ((def Message-Definition))
  "XLISP style definition, specialiers come from second arg of defmeth"
  (list (second (definition-form def))))


(setf (definer-class 'defmeth) 'Message-Definition)


