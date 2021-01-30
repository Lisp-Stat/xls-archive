;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: :Definitions -*-
;;;  File Name          :  sort.lisp
;;;  Version Number     :  1.2
;;;  Last Changed Date  :  93/08/20 At 19:57:39
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

;;; 6/12/98 RGA --- Modified to work with XLS.

;;;=======================================================

(in-package :Definitions)

;;;=======================================================
;;; sorting
;;;=======================================================

(defun definition-alpha< (def0 def1)

  "Order definitions alphabetically by name.
If the names are the same, call <definition<>
to resolve the ambiguity."

  (declare (type Definition def0 def1)
	   (:returns (type (Member t nil))))

  (let ((s0 (definition-symbol-name def0))
	(s1 (definition-symbol-name def1)))
    (declare (type String s0 s1))
    (cond ((string-lessp s0 s1) t)
	  ((string-equal s0 s1) (definition< def0 def1))
	  (t nil))))

;;;=======================================================

(defgeneric definition< (def0 def1)
	    (declare (type Definition def0 def1)
		     (:returns (type (Member t nil))))
	    (:documentation
	     "Resolve the ambiguity in alphabetic ordering for
multiple definitions with the same name."))

#|
(defmethod definition< ((def0 Definition)
			(def1 Definition))
  "The default method returns nil (not comparable)."
  (declare (type Definition def0 def1)
	   (:returns nil))
  (declare (type Definition def0 def1))
  nil)
|#
(defmethod definition< ((def0 Definition) def1)
  "The default method returns nil (not comparable)."
  (declare (type Definition def0 def1)
	   (:returns nil))
  (declare (type Definition def0 def1))
  nil)

#|
;; Not needed, setf definitions not supported.
(defmethod definition< ((def0 Lambda-List-Definition)
			(def1 Lambda-List-Definition))
  "Normal Functions come before setf functions."
  (declare (type Lambda-List-Definition def0 def1)
	   (:returns (type (Member t nil))))
  (and (not (setf-function-or-method? def0))
       (setf-function-or-method? def1)))
       
|#

#|
(defmethod definition< ((def0 Generic-Function-Definition)
			(def1 Method-Definition))
  "Generic Functions come before Methods."
  (declare (type Generic-Function-Definition def0)
	   (type Method-Definition def1)
	   (:returns t))
  (if (and (setf-function-or-method? def0)
	   (not (setf-function-or-method? def1)))
      nil
    t))
|#
(defmethod definition< ((def0 Generic-Function-Definition) def1)
					; (def1 Method-Definition))
  "Generic Functions come before Methods."
  (declare (type Generic-Function-Definition def0)
	   (type Method-Definition def1)
	   (:returns t))
  (cond ((and (typep def1 'object) (kind-of-p def1 Method-Definition))
	 t)
	((and (typep def1 'object) (kind-of-p def1 Message-Definition))
	 t)
	(t (call-next-method def0 def1)))
  )


(defmethod definition< ((def0 User-Type-Definition) def1)
					;(def1 Definition)) 
  "User defined types (deftype, defstruct, defclass) come before others."
  (declare (type User-Type-Definition def0)
	   (type Definition def1)
	   (:returns t))
  t)

;;;=======================================================
;;; This relies on the class & structure definitions being loaded.
;;; Currently we can't reliably get at method combination info
;;; (where's that MOP?), so we assume everything is standard.

(deftype Method-Combination-Type ()
  '(Member standard + and append list max min nconc or progn))

(deftype Method-Combination-Order ()
  '(Member :most-specific-first :most-specific-last))

#|
(defun method-combination-type (gf)

  "Get the method combination type. This should disappear when the MOP
is ready."

  (declare (type Generic-Function gf)
	   (:returns (type Method-Combination-Type)))

  (let ((mc (clos::generic-function-method-combination gf)))
    (declare (type clos::Method-Combination mc))
    (if (slot-boundp mc 'clos::type)
	(slot-value mc 'clos::type)
      ;; else
      'standard)))
|#
(defun method-combination-type (gf)
  "Get the method combination type. This is not used in XLS."
  'standard)

#|
(defun method-combination-order (gf)
  "Get the method combination order. This should disappear when the
MOP is ready."
  (declare (type Generic-Function gf)
	   (:returns (type Method-Combination-Order)))
  (let ((mc (clos::generic-function-method-combination gf)))
    (declare (type clos::Method-Combination mc))
    (if (and (slot-boundp mc 'clos::options)
	     (find :most-specific-last (slot-value mc 'clos::options)))
	:most-specific-last
      ;; else
      :most-specific-first)))
|#
(defun method-combination-order (gf)
  "Get the method combination order. This does not apply to XLS."
  :most-specific-first)


(defun subobjp (s0 s1)
  "This compares two type sepcifiers.  Either they are objects, in
which case we will need to use kind-of-p to do the test, or they are
types, in which case we can use subtypep."
  (let ((o0 (if (and (symbolp s0) (boundp s0))
		 (symbol-value s0)))
	 (o1 (if (and (symbolp s1) (boundp s1))
		 (symbol-value s1))))
    (if (and o0 o1 (typep o0 'object) (typep o1 'object))
	(kind-of-p o0 o1)
      ;; (subtypep s0 s1) but not avaliable in xlisp
      nil)))



(defun more-specific? (l0 l1)
  "Is the specializer list <l0> more specific than <l1>?
If they are not comparable, is it alphabetically less?"
  (declare (type List l0 l1)
	   (:returns (type (Member t nil))))
  (loop
    (when (or (null l0) (null l1)) (return nil))
    (let ((s0 (pop l0))
	  (s1 (pop l1)))
      (cond ((equal s0 s1) nil) ;; pop and try again
	    ((subobjp s0 s1) (return t))
	    ((subobjp s1 s0) (return nil))
	    ((string-lessp (format nil "~a" s0) (format nil "~a" s1))
	     (return t))
	    (t (return nil))))))

(defun less-specific? (l0 l1)
  "Is the specializer list <l0> more specific than <l1>?
If they are not comparable, is it alphabetically less?"
  (declare (type List l0 l1)
	   (:returns (type (Member t nil))))
  (loop
    (when (or (null l0) (null l1)) (return nil))
    (let ((s0 (pop l0))
	  (s1 (pop l1)))
      (cond ((equal s0 s1) nil) ;; pop and try again
	    ((and (listp s0) (eql 'eql (car s0))
		  (listp s1) (eql 'eql (car s1)))
	     (if (string-lessp (format nil "~a" (cdr s0))
			       (format nil "~a" (cdr s1)))
		 (return t)
	       (return nil)))
	    ((and (listp s0) (eql 'eql (car s0)))
	     (return t))
	    ((and (listp s1) (eql 'eql (car s1)))
	     (return nil))
	    ((subobjp s1 s0) (return t))
	    ((subobjp s0 s1) (return nil))
	    ((string-lessp (format nil "~a" s0) (format nil "~a" s1))
	     (return t))
	    (t (return nil))))))

#|
(defun method< (def0 def1)
  "Compute method ordering."
  (declare (type Method-Definition def0 def1)
	   (:returns (type (Member t nil))))
  (let* ((gf (fdefinition (definition-symbol def0)))
	 (l0 (definition-specializers def0))
	 (l1 (definition-specializers def1))
	 (mc-type  (method-combination-type gf))
	 (mc-order (method-combination-order gf))
	 (qual0 (definition-method-qualifier def0))
	 (qual1 (definition-method-qualifier def1)))
    (declare (type Generic-Function gf)
	     (type List l0 l1)
	     (type Method-Combination-Type mc-type)
	     (type Method-combination-Order mc-order))
    (case qual0
      (around (case qual1
		(around (more-specific? l0 l1))
		(otherwise t)))
      (before (case qual1
		(around nil)
		(before (more-specific? l0 l1))
		(otherwise t)))
      (after (case qual1
	       (after (less-specific? l0 l1))
	       (otherwise nil)))
      ;; else some sort of primary method
      (otherwise (case qual1
		   (around nil)
		   (before nil)
		   (after t)
		   (otherwise ;; must be same sort of primary method
		    (if (eq mc-type 'standard)
			;; always list standard primary methods
			;; as though :most-specific-last
			(less-specific? l0 l1)
		      ;; else for other method combination types,
		      ;; list in order of execution
		      (ecase mc-order
			(:most-specific-first (more-specific? l0 l1))
			(:most-specific-last (less-specific? l0 l1))))))))))
			|#

;;; Simplify this greatly as we don't have method qualifiers
(defun method< (def0 def1)
  "Compute method ordering."
  (declare (type Method-Definition def0 def1)
	   (:returns (type (Member t nil))))
  (let* 
	 ((l0 (definition-specializers def0))
	  (l1 (definition-specializers def1))
	  )
    (declare 
     (type List l0 l1))
    (less-specific? l0 l1)
    ))

#|
(defmethod definition< ((def0 Method-Definition) 
					(def1 Method-Definition))

  "Method ordering attempts to mimic calling order for combinable
methods and be alphabetic otherwise."

  (declare (type Method-Definition def0 def1)
	   (:returns (type (Member t nil))))

  (cond
   ((setf-function-or-method? def0)
    (if (setf-function-or-method? def1) (method< def0 def1) nil))
   ((setf-function-or-method? def1)
    t)
   (t
    (method< def0 def1))))
|#


(defmethod definition< ((def0 Method-Definition) def1)
					;(def1 Method-Definition))

  "Method ordering attempts to mimic calling order for combinable
methods and be alphabetic otherwise."

  (declare (type Method-Definition def0 def1)
	   (:returns (type (Member t nil))))

  (if (kind-of-p def1 Method-Definition)
      (method< def0 def1)
    (call-next-method def0 def1)))






