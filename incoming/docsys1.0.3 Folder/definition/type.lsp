;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: :Definitions -*-
;;;  File Name          :  type.lisp
;;;  Version Number     :  1.1
;;;  Last Changed Date  :  93/08/20 At 19:46:31
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

(defclass User-Type-Definition (Definition) ()
  (:documentation
   "An abstract super class for user defined types."))

#|
(defmethod definition-name->string ((def User-Type-Definition))
  "Type name strings should be capitalized."
  (declare (type  User-Type-Definition def)
	   (:returns (type String)))
  (let (#+:excl(excl:*print-nickname* t))
    (format nil "~:(~s~)" (definition-name def))))
|#

(defmethod definition-name->tag ((def User-Type-Definition))

  "The default method simply calls format on the <definition-name>,
printing in lower case."

  (declare (type Definition def)
	   (:returns (type String)))
  (substitute #\- #\?			;#\? looses in tag
	      (format nil "Type|~A" (definition-name->string def))))




(defmethod definition-usage ((def User-Type-Definition))
  "The example of use of a type definition is a call to <typep>."
  (declare (type Definition def)
	   (:returns (values (type String usage)
			     (type Fixnum indent))))
  (values (format nil "(typep x '~a)" (definition-name->string def))
	  0))

;;;-------------------------------------------------------

(defclass Type-Definition (User-Type-Definition Lambda-List-Definition) ()
  (:documentation
   "A definition class for <deftype>."))

(defmethod definition-class-nice-name ((def Type-Definition))
  (declare (:returns "Type"))
  "Type")

(defmethod definition-documentation ((def Type-Definition))
  "Type name strings should be capitalized."
  (declare (type  Type-Definition def)
	   (:returns (type String)))
  (let* ((body (nthcdr 3 (definition-form def)))
	 (1st (first body)))
    (when (and (stringp 1st) (rest body)) 1st)))

;;;-------------------------------------------------------

(defclass Class-Definition (User-Type-Definition) ()
  (:documentation
   "A definition class for <defclass>."))

(defmethod definition-class-nice-name ((def Class-Definition))
  (declare (:returns "Class"))
  "Class")

(defmethod definition-definee ((def Class-Definition))
  "Get the corresponding class object."
  (declare (type Class-Definition def)
	   (:returns (type Class)))
  ;(find-class (definition-symbol def))
  (symbol-value (definition-symbol def))
  )

(defmethod definition-documentation ((def Class-Definition))
  "Return the class documentation string or an empty string."
  (declare (type Class-Definition def)
	   (:returns (type String)))
  (find-documentation-option (nthcdr 4 (definition-form def))))

(defmethod definition-slots ((def Class-Definition))
  "Return the forms defining the slots of this class."
  (declare (type Class-Definition def)
	   (:returns (type List)))
  (fourth (definition-form def)))

(defmethod definition-methods ((def Class-Definition) &key all)
  "The default method returns ()."
  (declare (type Definition def)
	   (type (or T Nil) all)
	   (:returns ()))
  (let ((obj (definition-definee def)))
    (if all
	(send obj :method-selectors)
      (send obj :own-methods))))

(defmethod definition-parents ((def Class-Definition))
  "Returns the names of the direct superclasses."
  (declare (type Class-Definition def)
	   (:returns (type List)))
  (third (definition-form def)))

(defmethod definition-children ((def Class-Definition) &optional package)

  "Returns the names of the direct subclasses. This method requires
the class definition to be loaded and returns the names of all direct
subclasses, not just those that have corresponding definition
objects."

  (declare (type Class-Definition def)
	   (:returns (type List)))
;  (map 'List #'class-name
;       (class-direct-subclasses (definition-definee def)))
  ;; Note, need to do this the hard way in XLS.
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
    result)
  )

(defmethod definition-usage ((def Class-Definition))
  "Construct a string reflecting a typical function call."
  (declare (type Definition def)
	   (:returns (values (type String usage)
			     (type Fixnum indent))))
  (let* ((*print-case* :downcase)
	 #+:excl(excl:*print-nickname* t)
	 (name (definition-name->string def))
	 (slots (get-slot-initargs (definition-slots def))))
    (values
     (format nil "(send ~a :new ~@[ &key~{ ~a~}~])"
	     name slots)
     (length (format nil "(make-instance ~a " name)))))

;;;=======================================================

(defclass Structure-Definition (User-Type-Definition) ()
  (:documentation
   "A definition class for <defstruct>."))

(defmethod definition-class-nice-name ((def Structure-Definition))
  (declare (:returns "Structure"))
  "Structure")


(defmethod definition-name ((def Structure-Definition))

  "Getting the name of a defstruct requires a little analysis of the
second item in the definition form."

  (declare (type Structure-Definition def)
	   (:returns (type Symbol)))
  (let ((2nd (second (definition-form def))))
    (if (atom 2nd) 2nd (first 2nd))))

(defmethod definition-documentation ((def Structure-Definition))
  "Return the defstruct's doc string, or an empty string."
  (declare (type Structure-Definition def)
	   (:returns (type String)))
  (let ((3rd (third (definition-form def))))
    (if (stringp 3rd) 3rd "")))

(defmethod definition-slots ((def Structure-Definition))
  "Return the forms defining the slots of this structure."
  (declare (type Structure-Definition def)
	   (:returns (type List)))
  (mapcar #'process-slot
	  (nthcdr (if (stringp (third (definition-form def))) 3 2)
		  (definition-form def))))

(defun process-slot (slot-spec)
  (if (listp slot-spec)
      (if (eql 1 (length slot-spec))
	  (list (car slot-spec) nil)
	slot-spec)
    (list slot-spec nil)))

(defun structure-conc-name (def)
  (declare (type Structure-Definition def))
  (let ((2nd (second (definition-form def))))
    (if (atom 2nd)
	(concatenate 'String (string 2nd) "-")
      ;; else
      (let ((option (assoc :conc-name (rest 2nd))))
	(cond ((null option) (concatenate 'String (string (first 2nd)) "-"))
	      ((null (second option)) "")
	      (t (string (second option))))))))

(defun structure-constructor (def)
  (declare (type Structure-Definition def))
  (intern
   (let ((2nd (second (definition-form def))))
     (if (atom 2nd)
	 (concatenate 'String "MAKE-" (string 2nd))
       ;; else
       (let ((option (assoc :constructor (rest 2nd))))
	 (cond ((null option)
		(concatenate 'String "MAKE-" (string (first 2nd))))
	       ((null (second option)) nil)
	       (t (string (second option)))))))
   (symbol-package (definition-name def))))

(defun structure-copier (def)
  (declare (type Structure-Definition def))
  (intern
   (let ((2nd (second (definition-form def))))
     (if (atom 2nd)
	 (concatenate 'String "COPY-" (string 2nd))
       ;; else
       (let ((option (assoc :copier (rest 2nd))))
	 (cond ((null option)
		(concatenate 'String "COPY-" (string (first 2nd))))
	       ((null (second option)) nil)
	       (t (string (second option)))))))
   (symbol-package (definition-name def))))

(defun structure-predicate (def)

  "Compute the name of the predicate function, if one is automatically
created."

  (declare (type Structure-Definition def)
	   (:returns (type Symbol)))

  (intern

   (let ((2nd (second (definition-form def))))
     (if (atom 2nd)
	 (concatenate 'String (string 2nd) "-P")
       ;; else
       (let ((option (assoc :predicate (rest 2nd))))
	 (cond ((null option)
		(concatenate 'String (string (first 2nd)) "-P"))
	       ((null (second option)) nil)
	       (t (string (second option)))))))

   (symbol-package (definition-name def))))
