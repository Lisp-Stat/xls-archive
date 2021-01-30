;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File auxmacs.lisp: Macros Used in other Auxiliary Functions
;;;; Load this before anything else, then load auxfns.lisp.

(defmacro once-only (variables &rest body)
  "Returns the code built by BODY.  If any of VARIABLES
  might have side effects, they are evaluated once and stored
  in temporary variables that are then passed to BODY."
  (assert (every #'symbolp variables))
  (let ((temps nil))
    (dotimes (i (length variables)) (push (gensym) temps))
    `(if (every #'side-effect-free? (list .,variables))
         (progn .,body)
         (list 'let
	  ,`(list ,@(mapcar #'(lambda (tmp var)
				`(list ',tmp ,var))
		     temps variables))
	  (let ,(mapcar #'(lambda (var tmp) `(,var ',tmp))
			variables temps)
	    .,body)))))

(defun side-effect-free? (exp)
  "Is exp a constant, variable, or function,
  or of the form (THE type x) where x is side-effect-free?"
  (or (atom exp) (constantp exp)
      (starts-with exp 'function)
      (and (starts-with exp 'the)
           (side-effect-free? (third exp)))))

(defmacro funcall-if (fn arg)
  (once-only (fn)
    `(if ,fn (funcall ,fn ,arg) ,arg)))

(defmacro read-time-case (first-case &rest other-cases)
  "Do the first case, where normally cases are
  specified with #+ or possibly #- marks."
  (declare (ignore other-cases))
  first-case)

(defun rest2 (x)
  "The rest of a list after the first TWO elements."
  (rest (rest x)))

(defun find-anywhere (item tree)
  "Does item occur anywhere in tree?"
  (if (atom tree)
      (if (eql item tree) tree)
      (or (find-anywhere item (first tree))
          (find-anywhere item (rest tree)))))

(defun starts-with (list x)
  "Is x a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(provide "auxmacs")
