;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: :Definitions -*-
;;;  File Name          :  condition.lisp
;;;  Version Number     :  1.4
;;;  Last Changed Date  :  93/09/23 At 00:13:45
;;;  Created Date       :  9/23/93
;;;
;;; Copyright 1993. Russell G. Almond. Based on work of John A.
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
;;; 8/18/93 RGA Added Conditions to list of defs.
;;;=======================================================

(in-package :definitions)
(defclass Condition-Definition (Class-Definition) ()
  (:documentation
   "A definition class for <define-condition>."))

(defmethod definition-definee ((def Condition-Definition))
  "Get the corresponding class object."
  (declare (type Condition-Definition def)
	   (:returns (type Class)))
  (find-class (definition-symbol def)))

(defmethod definition-documentation ((def Condition-Definition))
  "Return the class documentation string or an empty string."
  (declare (type Condition-Definition def)
	   (:returns (type String)))
  (find-documentation-option (nthcdr 4 (definition-form def))))

(defmethod definition-slots ((def Condition-Definition))
  "Return the forms defining the slots of this class."
  (declare (type Condition-Definition def)
	   (:returns (type List)))
  (fourth (definition-form def)))

(defmethod definition-parents ((def Condition-Definition))
  "Returns the names of the direct superclasses."
  (declare (type Condition-Definition def)
	   (:returns (type List)))
  (third (definition-form def)))

(defmethod definition-children ((def Condition-Definition) &optional package)

  "Returns the names of the direct subclasses. This method requires
the class definition to be loaded and returns the names of all direct
subclasses, not just those that have corresponding definition
objects."

  (declare (type Condition-Definition def)
	   (:returns (type List)))
  (map 'List #'class-name
       (class-direct-subclasses (definition-definee def))))


(setf (definer-class 'define-condition) 'Condition-Definition)

(defmethod definition-class-nice-name ((def Condition-Definition))
  (declare (:returns "KR:Schema"))
  (let ((name (definition-name def)))
    (cond ((subtypep name 'Error) "Error")
	  ((subtypep name 'Warning) "Warning")
	  (t "Condition"))))


(defmethod definition-usage ((def Condition-Definition))
  "Construct a string reflecting a typical function call."
  (declare (type Definition def)
	   (:returns (values (type String usage)
			     (type Fixnum indent))))
  (let* ((*print-case* :downcase)
	 #+:excl(excl:*print-nickname* t)
	 (name (definition-name def))
	 (signaler (cond ((subtypep name 'Error) "error")
			 ((subtypep name 'Warning) "warn")
			 (t "signal")))
	 (slots (get-slot-initargs (definition-slots def))))
    (values
     (format nil "(~A ~s ~@[ &key~{ ~a~}~])" signaler name slots)
     (length (format nil "(~A " signaler)))))



(defun get-slot-initargs (slot-specifications)
  (mapcan #'get-slot-initarg-list slot-specifications))

(defun get-slot-initarg-list (slot-spec)
  (labels ((gsil-aux (slot-spec result)
	     (if (null slot-spec) result
	       (gsil-aux (cddr slot-spec) 
			 (if (eql :initarg (car slot-spec))
			     (cons (cadr slot-spec) result)
			   result)))))
    (nreverse (gsil-aux (cdr slot-spec) nil))))

		   
	     



