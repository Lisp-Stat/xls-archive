;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: :Definitions -*-
;;;  File Name          :  defs.lisp
;;;  Version Number     :  1.2
;;;  Last Changed Date  :  93/08/20 At 19:49:04
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

;(declaim (declaration :returns))

;;;=======================================================

(defun find-documentation-option (form)
  (let ((doc (second 
	      (find-if #'(lambda (item)
			   (and (listp item)
				(eq (first item) :documentation)))
		       form))))
    (if (stringp doc) doc "")))

;;;=======================================================

  "A <check-type> that takes arguments more like declarations, eg,
  (declare (type Integer x y))"

(defmacro type-check (type &rest args)
  (let* ((args (remove-duplicates args))
 	 (variables (remove-if #'constantp args))
	 (runtime-checks (mapcar #'(lambda (arg)
				     `(xlos:my-check-type ,arg ,type))
				 variables)))
    ;; test constants at macro expand time:
    (dolist (constant (remove-if-not #'constantp args))
      ;; need to <eval> because <constant> might be a <defconstant>
      ;; rather than a literal constant
      (assert (xlos:mytypep (eval constant) type)))
    (unless (null runtime-checks) `(progn ,@runtime-checks))))

;;;-------------------------------------------------------

(defmacro declare-check (&rest decls)

  "This macro generates type checking forms and has a syntax like <declare>.
Unfortunately, we can't easily have it also generate the declarations."

  `(progn
     ,@(mapcar #'(lambda (d) `(type-check ,(second d) ,@(cddr d)))
	       ;; ignore non-type declarations
	       (remove-if-not #'(lambda (d) (eq (first d) 'type))
			      decls))))



