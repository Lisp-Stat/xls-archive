;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: :Definitions -*-
;;;  File Name          :  pack.lisp
;;;  Version Number     :  1.2
;;;  Last Changed Date  :  93/08/20 At 19:53:23
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

(defclass Package-Definition (Definition) ()
  (:documentation
   "A definition class for <defpackage>."))


(defmethod definition-class-nice-name ((def Package-Definition))
  (declare (:returns "Package"))
  "Package")


(defmethod definition-documentation ((def Package-Definition))
  "Returns the value of the :documentation option or an empty string."
  (declare (type Package-Definition def)
	   (:returns (type String)))
  (find-documentation-option (nthcdr 2 (definition-form def))))

(defmethod definition-name->string ((def Package-Definition))
  "Package name strings should be capitalized."
  (declare (type Package-Definition def)
	   (:returns (type String)))
  (format nil "~:(~s~)" (definition-name def)))

(defmethod definition-name->tag ((def Package-Definition))

  "The default method simply calls format on the <definition-name>,
printing in lower case."

  (declare (type Definition def)
	   (:returns (type String)))
  (substitute #\- #\?			;#\? looses in tag
	      (format nil "Pack|~A" (definition-name->string def))))

(defmethod definition-symbol ((def Package-Definition))

  "`name' could be a string, so need to intern it in keyword package
to make package symbol."

  (declare (type Definition def)
	   (:returns (type Symbol)))
  (let ((name (definition-name def)))
    (if (symbolp name) name
      (intern name :keyword))))



(defmethod definition-usage ((def Package-Definition))
  "The example of package use is a call to <in-package>."
  (declare (type Definition def)
	   (:returns (values (type String usage)
			     (type Fixnum indent))))
  (values (format nil "(in-package ~a)" (definition-name->string def))
	  0))

