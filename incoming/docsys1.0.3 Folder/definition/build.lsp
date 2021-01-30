;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: :Definitions -*-
;;;  File Name          :  build.lisp
;;;  Version Number     :  1.2
;;;  Last Changed Date  :  93/08/20 At 19:47:35
;;;  Created Date       :  8/20/93
;;;
;;; Copyright 1991. John Alan McDonald. All Rights Reserved. 
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
;;;=======================================================

(in-package :Definitions)

;;;=======================================================
;;; Translating definer names to class names:
;;;=======================================================

(defvar *definer-class* (make-hash-table :test #'eq)

  "A table for mapping definers (eg. defun) to classes of definition
objects.")

(defun definer-class (definer)

  "Returns the name of the definition class for the <definer> symbol.

The predefined definer classes are: 
Class-Definition for defclass,
Constant-Definition for defconstant,
Function-Definition for defun,
Generic-Function-Definition for defgeneric,
Macro-Definition for defmacro,
Method-Definition for defmethod,
Package-Definition for defpackage,
Parameter-Definition for defparameter,
Setf-Definition for defsetf,
Structure-Definition for defstruct,
Type-Definition for deftype,
and
Variable-Definition for defvar."

  (declare (type Symbol definer)
	   (:returns (type Symbol class-name)))

  (gethash (symbol-name definer) *definer-class* nil))

(defsetf definer-class (definer) (class-name)
  "Assign a definer class name to a definer symbol."
  `(setf (gethash (symbol-name ,definer) *definer-class*) ,class-name))

(eval-when (load eval)
  (setf (definer-class 'defclass)     'Class-Definition)
  (setf (definer-class 'defconstant)  'Constant-Definition)
  (setf (definer-class 'defun)        'Function-Definition)
  (setf (definer-class 'defgeneric)   'Generic-Function-Definition)
  (setf (definer-class 'defmacro)     'Macro-Definition)
  (setf (definer-class 'defmethod)    'Method-Definition)
  (setf (definer-class 'defpackage)   'Package-Definition)
  (setf (definer-class 'defparameter) 'Parameter-Definition)
  (setf (definer-class 'defsetf)      'Setf-Definition)
  (setf (definer-class 'defstruct)    'Structure-Definition)
  (setf (definer-class 'deftype)      'Type-Definition)
  (setf (definer-class 'defvar)       'Variable-Definition))

;;;=======================================================
;;; constructing definition objects
;;;=======================================================

(defgeneric make-slot-accessor-definitions (def slot-spec path)
	    (declare (type User-Type-Definition def)
		     (type List slot-spec)
		     (type Pathname path)
		     (:returns (type List definitions)))
	    (:documentation
	     "Make definition objects corresponding to automatically
generated accessor functions."))

#|
(defmethod make-slot-accessor-definitions ((def Class-Definition)
					   slot-spec path)
  "Make definition objects corresponding to automatically generated
accessor functions for a class."
  (declare (type Class-Definition def)
	   (type List slot-spec)
	   (type Pathname path)
	   (:returns (type List definitions)))
  (let* ((name (definition-name def))
	 (options (rest slot-spec))
	 (defs ())
	 (doc-string (getf options :documentation ""))
	 (type (getf options :type T)))
    (loop
      (let ((key (first options))
	    (val (second options)))
	(case key
	  (:accessor
	   (push
	    (make-instance 'Method-Definition
	      :definition-form
	      `(defmethod (setf ,val) (new-value (x ,name)) ,doc-string)
	      :definition-path path)
	    defs)
	   (push
	    (make-instance 'Method-Definition
	      :definition-form
	      `(defmethod ,val ((x ,name))
		 ,doc-string
		 (declare (type ,name x)
			  (:returns (type ,type))))
	      :definition-path path)
	    defs))
	  (:reader
	   (push
	    (make-instance 'Method-Definition
	      :definition-form
	      `(defmethod ,val ((x ,name))
		 ,doc-string
		 (declare (type ,name x)
			  (:returns (type ,type))))
	      :definition-path path)
	    defs))
	  (:writer
	   (push
	    (make-instance 'Method-Definition
	      :definition-form
	      `(defmethod (setf ,val) (new-value (x ,name))
		 ,doc-string)
	      :definition-path path)
	    defs))))
      (setf options (cddr options))
      (when (null options) (return defs)))))
|#

;;; Need to change to use XLS style messages.

(defmethod make-slot-accessor-definitions ((def Class-Definition)
					   slot-spec path)
  "Make definition objects corresponding to automatically generated
accessor functions for a class."
  (declare (type Class-Definition def)
	   (type List slot-spec)
	   (type Pathname path)
	   (:returns (type List definitions)))
  (let* ((name (definition-name def))
	 (options (rest slot-spec))
	 (defs ())
	 (doc-string (getf options :documentation ""))
	 (type (getf options :type T)))
    (loop
      (let ((key (first options))
	    (val (second options)))
	(case key
	  (:accessor
	   (push
	    (make-instance 'Message-Definition
	      :definition-form
	      `(defmeth ,name ,val (&optional new-value) ,doc-string
			(declare (type ,type new-value)))
	      :definition-path path)
	    defs))
	  (:reader
	   (push
	    (make-instance 'Method-Definition
	      :definition-form
	      `(defmeth ,name ,val () 
		 ,doc-string
		 (declare (:returns (type ,type))))
	      :definition-path path)
	    defs))
	  (:writer
	   (push
	    (make-instance 'Method-Definition
	      :definition-form
	      `(defmethod ,name (setf ,val) (new-value)
			  ,doc-string 
			  (declare (type ,type new-value)))
	      :definition-path path)
	    defs))))
      (setf options (cddr options))
      (when (null options) (return defs)))))


(defmethod make-slot-accessor-definitions ((def Structure-Definition)
					   slot-spec path)
  "Make definition objects corresponding to automatically generated
accessor functions for a structure."
  (declare (type Structure-Definition def)
	   (type List slot-spec)
	   (type Pathname path)
	   (:returns (type List definitions)))
  (let* ((name (definition-name def))
	 (slot (intern (concatenate 'String
			 (structure-conc-name def)
			 (string (first slot-spec)))
		       (symbol-package name)))
	 (type (getf slot-spec :type T))
	 (read-only? (getf slot-spec :read-only nil))
	 (defs (list (make-instance 'Function-Definition
		       :definition-form
		       `(defun ,slot (x)
			  ,(format nil "Accessor for ~s." slot)
			  (declare (type ,name x)
				   (:returns (type ,type))))
		       :definition-path path))))
    (unless read-only?
      (push (make-instance 'Setf-Definition
	      :definition-form
	      `(defsetf ,slot (x) (new-value)
		 ,(format nil "Set the ~s attribute." slot)
		 (declare (type ,name x)
			  (type ,type new-value)))
	      :definition-path path)
	    defs))
    defs))


;;;-------------------------------------------------------

(defgeneric make-subdefinitions (def path)

  (declare (type Definition def)
	   (type Pathname path)
	   (:returns (type List definitions)))

  (:documentation
   "Make definition objects for definitions automatically generated
by the evaluation of the form corresponding to <def>.
An example of a subdefinition is a slot accessor function
automatically generated by a class definition."))

(defmethod make-subdefinitions ((def Definition) path)

  "The default method for <make-subdefinitions> returns ()."

  (declare (type Definition def)
	   (type Pathname path)
	   (:returns ()))
  path
  ())

(defmethod make-subdefinitions ((def Class-Definition) path)

  "The method for classes returns a list of definition objects for the
slot accessor functions."

  (declare (type Definition def)
	   (type Pathname path)
	   (:returns (type List definitions)))

  (mapcan #'(lambda (spec) (make-slot-accessor-definitions def spec path))
	  (definition-slots def)))

(defmethod make-subdefinitions ((def Structure-Definition) path)

  "The method for structures returns a list of definition objects for
the automatically generated constructor, copier, and predicate
functions, if they are generated, and the slot accessor functions."

  (declare (type Definition def)
	   (type Pathname path)
	   (:returns (type List definitions)))

  (let ((name (definition-name def))
	(constructor (structure-constructor def))
	(copier (structure-copier def))
	(predicate (structure-predicate def))
	(defs (mapcan #'(lambda (spec)
			  (make-slot-accessor-definitions def spec path))
		      (definition-slots def))))
    (when constructor
      (push (make-instance 'Function-Definition
	      :definition-form
	      `(defun ,constructor (&rest initargs)
		 ,(format nil "The constructor function for ~s." name)
		 (declare (:returns (type ,name))))
	      :definition-path path)
	    defs))
    (when copier
      (push (make-instance 'Function-Definition
	      :definition-form
	      `(defun ,copier (x)
		 ,(format nil "The copier function for ~s." name)
		 (declare (type ,name x)
			  (:returns (type ,name))))
	      :definition-path path)
	    defs))
    (when predicate
      (push (make-instance 'Function-Definition
 	      :definition-form
	      `(defun ,predicate (x)
		 ,(format nil "The equality predicate for ~s." name)
		 (declare (:returns (type (Member t nil)))))
	      :definition-path path)
	    defs))
    defs))

;;;-------------------------------------------------------

(defun make-definitions (form path)

  "Make and return a list of the definition objects corresponding to
the result of evaluating <form> (which was read from the file
corresponding to <path>."

  (declare (type List form)
	   (type Pathname path)
	   (:returns (type List definitions)))

  (let ((1st (first form)))
    (case 1st
      ((block multiple-value-prog1 progn prog1 prog2 tagbody)
       (mapcan #'(lambda (form) (make-definitions form path)) (rest form)))
      ((eval-when let let* prog prog*)
       (mapcan #'(lambda (form) (make-definitions form path)) (nthcdr 2 form)))
      ((progv)
       (mapcan #'(lambda (form) (make-definitions form path)) (nthcdr 3 form)))
      (otherwise
       (let ((class-name (definer-class (first form))))
	 (when class-name
	   (let ((def (make-instance class-name
			:definition-form form
			:definition-path path)))
	     (cons def (make-subdefinitions def path)))))))))



