;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: :XLOS -*-

;;;  File Name          :  defclass.lisp
;;;  Version Number     :  1.0
;;;  Last Changed Date  :  6/9/98
;;;  Created Date       :  6/9/98
;;;
;;; Copyright 1998. Russell G. Almond.
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and no warranty---about the
;;; software, its performance, or its conformity to any
;;; specification---is given or implied.
;;; 
;;;=======================================================

(defpackage "XLOS" 
  (:use "XLISP")
  (:export "DEFGENERIC" "DEFMETHOD"))

(in-package :xlos)

;;; This is a quick and dirty translation routine for translating code
;;; written in CLOS into XLS.  It only supports generic functions
;;; which dispatch on one argument and methods for object types.
;;; Also, it doesn't support method qualifiers or generic functions
;;; whose names are not a symbol.  It doesn't support elaborate method
;;; combination types.  Finally, it makes no attempt to
;;; check for conformity between method and generic function.

;;; Basically, the way it works is that a (defgeneric) macro creates a
;;; function which sends the object the message whose name is the same
;;; as the function.  The defmethod macro produces a corresponding
;;; defmeth form.

(defmacro defgeneric (name arglist &body options)
  "This macro creates a ``generic function'', a call to send with the
appropriate arguments."
  (unless (symbolp name)
    (error "Don't support (setf xxx) generic functions."))
  (unless (listp arglist)
    (error "Don't support method qualifiers."))
  (let ((declare-form
	 (find 'declare options :key #'car))
	(documentation-form
	 (find ':documentation options :key #'car))
	(object (car arglist))
	(rest-arg (get-rest-arg (cdr arglist)))
	(args (clean-arg-list (cdr arglist)))
	(forms nil))
    (unless rest-arg
      ;; Okay, we don't want to evaluate the keyword args yet because
      ;; we wan't the methods defaults to work.  So we need to fake
      ;; an &rest arg.
      (setq rest-arg (gensym 'rest))
      (setq arglist (munge-arg-list arglist rest-arg))
      )
    ;; Defined with &rest, invoke with apply	
    (push `(apply #'send ,object ',name ,object ,.args ,rest-arg)
	  forms)
    (if declare-form
	(push declare-form forms))
    (if documentation-form
	(push (cadr documentation-form) forms))
    `(defun ,name ,arglist ,@forms)
    ))


(defun get-rest-arg (arglist)
  "This function finds the &rest argument (if any) from an arglist and
extracts it."
  (let ((tail (member '&rest arglist)))
    ;; if &rest is found, tail should be lambda-list starting from &rest
    (if tail (second tail)
      nil)))

(defun clean-arg-list (arglist &optional pos-args)
  "This function strips of the extranious information in the lambda
list and returns a list of positional (required and optional) arugments."
  (cond ((null arglist)
	 (values (reverse pos-args)))
	;; (var [initform [svar]]) type arg
	((listp (car arglist))
	 (clean-arg-list (cdr arglist) (cons (caar arglist) pos-args)))
	((symbolp (car arglist))
	 (case (car arglist)
	   ((&optional)			;&optional section,
					;keep going
	    (clean-arg-list (cdr arglist) pos-args))
	   ((&rest &key &aux)		;&rest, &key or &aux 
					;We are done
	    (values (reverse pos-args)))
	   (t				;var style arg
	    (clean-arg-list (cdr arglist) (cons (car arglist) pos-args)))
	   ))
	(t (error "Bad arglist"))))

(defun munge-arg-list (arglist rest-arg)
  "This function adds an &rest arg at the appropriate place."
  (let ((pos 
	 (cond ((position '&key arglist))
	       ((position '&aux arglist))
	       ((length arglist)))))
    (append (subseq arglist 0 pos) 
	    (list '&rest rest-arg)
	    (subseq arglist pos))))



(defun key-arg-list (arglist)
  "This returns a list of the form keyword1 var1 keyword2 var2 ... for
  building the function call without using apply."
  (key-arg-list-aux2
   (key-arg-list-aux (cdr (member '&key arglist)) nil)))


(defun key-arg-list-aux (arglist key-args)
  "This function returns a list of (:keyword var) pairs
for fixing the &key args (without using an &rest)."
  (cond ((null arglist) key-args)
	;; (var [initform [svar]]) type arg
	((listp (car arglist))
	 (key-arg-list-aux (cdr arglist) (cons (caar arglist) key-args)))
	((symbolp (car arglist))
	 (case (car arglist)
	   ((&optional &rest &key)	;&optional section,
					;keep going
	    ;;duplicate keyword
	    (error "Bad arglist"))
	   ((&allow-other-keys &aux)	;&aux or &allow-other-keys
					;We are done
	    key-args)
	   (t				;var style arg
	    (key-arg-list-aux (cdr arglist) (cons (car arglist) key-args)))
	   ))
	(t (error "Bad arglist"))))

(defun key-arg-list-aux2 (key-args)
  "This function returns flattend list of :keyword var pairs
for fixing the &key args (without using an &rest)."
  (let (keyword var)
    (cond ((null key-args) nil)
	  ((listp (car key-args))
	   ;; ((keyword var) ...) form
	   (setq keyword (first (car key-args)) var (second (car key-args))))
	  ((symbolp (car key-args))
	   (setq var (car key-args))
	   (setq keyword (intern (symbol-name var) keyword)))
	  (t (error "Bad arglist")))
    (cons keyword (cons var (key-arg-list-aux2 (cdr key-args))))))

(defmacro defmethod (name ((obj-arg proto-name) . otherargs) &body body)
  "This translates a defmethod form into the approprate defmeth form."
  (let ((prototype (if (symbolp proto-name) (symbol-value proto-name)
		     proto-name)))
    (unless (symbolp name)
      (error "Don't support (setf xxx) methods."))
    (unless (typep prototype 'object)
      (error "Only support methods for objects.  Arglist ((~S ~S)) ~S)"
	     obj-arg prototype otherargs))
    (when (and (car otherargs) (listp (car otherargs)))
      (error "Only support dispatch on one argument."))
    `(defmeth ,prototype ,name ,(cons obj-arg otherargs) ,.body))
  )