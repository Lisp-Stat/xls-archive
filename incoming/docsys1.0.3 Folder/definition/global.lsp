;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: :Definitions -*-
;;;  File Name          :  global.lisp
;;;  Version Number     :  1.2
;;;  Last Changed Date  :  93/08/20 At 19:50:59
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

(defclass Global-Variable-Definition (Definition) ()
  (:documentation
   "An abstract super class for global variable definitions."))

(defmethod definition-name->tag ((def Global-Variable-Definition))

  "The default method simply calls format on the <definition-name>,
printing in lower case."

  (declare (type Definition def)
	   (:returns (type String)))
  (substitute #\- #\?			;#\? looses in tag
	      (format nil "Var|~A" (definition-name->string def))))

(defun definition-initial-value (def)
  "The initial value supplied for a global variable (or constant) definition."
  (declare (type Definition def)
	   (:returns (type T)))
  (if (typep def 'Global-Variable-Definition)
      (third (definition-form def))
    ;; else
    ()))

(defclass Constant-Definition  (Global-Variable-Definition) ()
  (:documentation
   "A definition class for <defconstant>."))

(defmethod definition-class-nice-name ((def Constant-Definition))
  (declare (:returns "Constant"))
  "Constant")

(defclass Parameter-Definition (Global-Variable-Definition) ()
  (:documentation
   "A definition class for <defparameter>."))

(defmethod definition-class-nice-name ((def Parameter-Definition))
  (declare (:returns "Parameter"))
  "Parameter")

(defclass Variable-Definition  (Global-Variable-Definition) ()
  (:documentation
   "A definition class for <defvar>."))

(defmethod definition-class-nice-name ((def Variable-Definition))
  (declare (:returns "Variable"))
  "Variable")

