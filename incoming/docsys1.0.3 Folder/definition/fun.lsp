;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: :Definitions -*-
;;;  File Name          :  fun.lisp
;;;  Version Number     :  1.3
;;;  Last Changed Date  :  95/10/17 At 12:33:18
;;;  Created Date       :  10/17/95
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
;;;
;;; 6/9/92 RGA Added announcement to list of defs.
;;;=======================================================

(in-package :Definitions)

;;;=======================================================

(defun lambda-list-whole-arg (lambda-list)
  "The name of the &whole arg in a (macro's) lambda list."
  (declare (type List lambda-list)
	   (:returns (type Symbol)))
  (when (eq (first lambda-list) '&whole)
      (second lambda-list)))

(defun lambda-list-required-args (lambda-list)

  "A list of the required args (with specializers when given)."

  (declare (type List lambda-list)
	   (:returns (type List)))
  (when (eq (first lambda-list) '&whole)
    (setf lambda-list (nthcdr 2 lambda-list)))
  (let ((required-args ()))
    (declare (type List required-args))
    (loop (when (or (null lambda-list)
		    (member (first lambda-list) lambda-list-keywords))
	    (return (nreverse required-args)))
      (push (pop lambda-list) required-args))))

(defun lambda-list-specializers (lambda-list)
  "A list of the specializers for the required args, with T given for any
unspecialized args."
  (declare (type List lambda-list)
	   (:returns (type List)))
  (mapcar #'(lambda (item) (if (atom item) t (second item)))
	  (lambda-list-required-args lambda-list)))

(defun lambda-list-required-arg-names (lambda-list)
  "a list of the name oif the required args (specializers are stripped off.)"
  (declare (type List lambda-list)
	   (:returns (type List)))
  (mapcar #'(lambda (item) (if (atom item) item (first item)))
	  (lambda-list-required-args lambda-list)))

(defun lambda-list-optional-args (lambda-list)
  "A list of the &optional args, with default values, etc."
  (declare (type List lambda-list)
	   (:returns (type List)))
  (let ((optional-args ()))
    (declare (type List optional-args))
    (loop (when (or (null lambda-list)
		    (eq (pop lambda-list) '&optional))
	    (return)))
    (loop (when (or (null lambda-list)
		    (member (first lambda-list) lambda-list-keywords))
	    (return (nreverse optional-args)))
      (push (pop lambda-list) optional-args))))

(defun lambda-list-optional-arg-names (lambda-list)
  "A list of the names only of the &optional args."
  (declare (type List lambda-list)
	   (:returns (type List)))
  (mapcar #'(lambda (item) (if (atom item) item (first item)))
	  (lambda-list-optional-args lambda-list)))

(defun lambda-list-rest-arg-name (lambda-list)
  "The name of the &rest or &body arg."
  (declare (type List lambda-list)
	   (:returns (type Symbol)))
  (loop (when (or (null lambda-list)
		  (member (pop lambda-list) '(&rest &body)))
	  (return (first lambda-list)))))

(defun lambda-list-keyword-args (lambda-list)
  "A list of the &keyword args, with default values, etc."  
  (declare (type List lambda-list)
	   (:returns (type List)))
  (let ((keyword-args ()))
    (declare (type List keyword-args))
    (loop (when (or (null lambda-list)
		    (eq (pop lambda-list) '&key))
	    (return)))
    (loop (when (or (null lambda-list)
		    (member (first lambda-list) lambda-list-keywords))
	    (return (nreverse keyword-args)))
      (push (pop lambda-list) keyword-args))))

(defun lambda-list-keyword-arg-names (lambda-list)
  "A list of the names only of the &keyword args."
  (declare (type List lambda-list)
	   (:returns (type List)))
  (mapcar #'(lambda (item) (if (atom item) item (first item)))
	  (lambda-list-keyword-args lambda-list)))

(defun lambda-list-arg-names (lambda-list)
  "A list of all the arg names."
  (declare (type List lambda-list)
	   (:returns (type List)))
  (concatenate 'List
   (lambda-list-required-arg-names lambda-list)
   (lambda-list-optional-arg-names lambda-list)
   (lambda-list-keyword-arg-names lambda-list)))

;;;=======================================================

(defclass Lambda-List-Definition (Definition) ()
  (:documentation
   "An abstract super class for definitions that include lambda lists."))

(defmethod definition-name->tag ((def Lambda-List-Definition))

  "The default method simply calls format on the <definition-name>,
printing in lower case."

  (declare (type Definition def)
	   (:returns (type String)))
  (substitute #\- #\?			;#\? looses in tag
	      (format nil "Lambda|~A" (definition-name->string def))))


(defgeneric definition-body (def)
  (declare (type Lambda-List-Definition def)
	   (:returns (type List form)))
  (:documentation "This generic function extracts the body from a
  lambda list definition.  It is used by a number of other methods to
  provide for different &body syntaxes."))

(defmethod definition-body ((def Lambda-List-Definition))
  "Default method extracts (nthcdr 3)"
  ;; Most defs have (def<xxx> name args . body)
  (nthcdr 3 (definition-form def)))

(defun setf-function-or-method? (def)
  (declare (type Lambda-List-Definition def)
	   (:returns (type (Member T Nil))))
  (let ((name (definition-name def)))
    (cond ((atom name) nil)
	  ((listp name)
	   (ecase (first name)
	     (setf t)
	     (:method (if (atom (second name)) nil t)))))))

(defmethod definition-lambda-list ((def Lambda-List-Definition))
  "The lambda list is the third item in most definitions."
  (declare (type Lambda-List-Definition def)
	   (:returns (type List)))
  (third (definition-form def)))


(defmethod definition-documentation ((def Lambda-List-Definition))

  "The documentation string is the first string that comes before the
first non-<declare> form at the top of the function body, unless it's
the returned value."

  (declare (type Lambda-List-Definition def)
	   (:returns (type String)))

  (let ((body (definition-body def)))
    (loop
      (let ((item (pop body)))
	(cond ((and body (stringp item)) (return item))
	      ((not (and (listp item) (eql (first item) 'declare)))
	       (return "")))))))

(defmethod definition-declarations ((def Lambda-List-Definition))

  "Returns all the decl-specs from <declare> forms that come before
the first non-string non-<declare> form in the function body.  Forms
that begin with <declare-check> are also treated as declarations."

  (declare (type Lambda-List-Definition def)
	   (:returns (type List)))

  (let ((body (definition-body def))
	(decls ()))
    (loop
      (let ((item (pop body)))
	(cond ((stringp item) t)
	      ((and
		(listp item)
		(or (eq (first item) 'declare)
		    ;; also get az type checking "declare"`s
		    (and (symbolp (first item))
			 (string-equal (symbol-name (first item))
				       "DECLARE-CHECK"))))
	       (setf decls (concatenate 'List (rest item) decls)))
	      (t (return)))))
    decls))

(defmethod definition-arg-types ((def Lambda-List-Definition))

  "Extract a list of lists of length 2, where each sublist is a name
--- type pair. The types are gotten first from the arg specializers,
if there are any, and are overridden by any top level type
declarations."

  (declare (type Lambda-List-Definition def)
	   (:returns (type List)))
  (let* ((lambda-list (definition-lambda-list def))
	 (arg-types (concatenate 'List
		      (mapcar #'(lambda (a) (if (atom a) (list a T) a))
			      (lambda-list-required-args lambda-list))
		      (mapcar #'(lambda (a) (list a T))
			      (lambda-list-optional-arg-names lambda-list))
		      (unless (null (lambda-list-rest-arg-name lambda-list))
			(list (list (lambda-list-rest-arg-name lambda-list) T)))
		      (mapcar #'(lambda (a) (list a T))
			      (lambda-list-keyword-arg-names lambda-list)))))
    (dolist (decl (definition-type-declarations def))
      (let ((type (second decl)))
	(dolist (arg (cddr decl))
	  (let ((entry (assoc arg arg-types)))
	    (when entry (setf (second entry) type))))))
    arg-types))

(defmethod definition-usage ((def Lambda-List-Definition))
  "Construct a string reflecting a typical function call."
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
    (when (and (listp name) (eq (first name) :method))
      (setf name (second name)))
    (cond ((atom name)
	   (values
	    (format nil
	     "(~a~{ ~a~}~@[ &optional~{ ~a~}~]~@[ &rest ~a~]~@[ &key~{ ~a~}~])"
	     (princ-with-nicknames name) required-arg-names optional-arg-names
	     rest-arg-name keyword-arg-names)
	    (length (format nil "(~A " (princ-with-nicknames name)))))
	  ((eq (first name) 'setf)
	   (values 
	    (format nil
	     "(setf (~A~{ ~a~}~@[ &optional~{ ~a~}~]~@[ &key~{ ~a~}~]) ~a)"
	     (princ-with-nicknames (second name))
	     (rest required-arg-names)
	     optional-arg-names keyword-arg-names
	     (first required-arg-names))
	   (length (format nil "(setf (~A " (princ-with-nicknames (second name))))))
	  (t
	   (error "Don't know how to make a usage string for ~a" def)))
    ))

;;;=======================================================

(defclass Function-Definition (Lambda-List-Definition) ()
  (:documentation "A definition class for <defun>."))

(defmethod definition-class-nice-name ((def Function-Definition))
  (declare (:returns "Function"))
  "Function")

;;;-------------------------------------------------------

(defclass Macro-Definition (Lambda-List-Definition) ()
  (:documentation "A definition class for <defmacro>."))

(defmethod definition-class-nice-name ((def Macro-Definition))
  (declare (:returns "Macro"))
  "Macro")

(defmethod definition-usage ((def Macro-Definition))
  "Construct a string for a typical call to the macro."
  (declare (type Definition def)
	   (:returns (values (type String usage)
			     (type Fixnum indent))))
  (let* ((*print-case* :downcase)
	 (*print-pretty* t)
	 #+:excl(excl:*print-nickname* t)
	 (name (definition-name def))
	 (lambda-list (definition-lambda-list def)))
    ;;(format nil "(~s~{ ~a~})" name lambda-list)
    (values
     (with-output-to-string (out)
       (format out "(~a" (princ-with-nicknames name))
       (when lambda-list (format out "~%~{ ~a~}" lambda-list))
       (format out ")")
       out)
     (length (format nil "(~A " (princ-with-nicknames name))))))

(defmethod definition-arg-types ((def Macro-Definition))
  "I haven't figured out a good way to get at the equivalent of arg
type declarations for macros, so this just returns nil."
  (declare (type Macro-Definition def)
	   (:returns (type String)))
  ())

;;;-------------------------------------------------------

(defclass Setf-Definition (Lambda-List-Definition) ()
  (:documentation "A definition class for <defsetf>."))

(defmethod definition-name->tag ((def Setf-Definition))

  "The default method simply calls format on the <definition-name>,
printing in lower case."

  (declare (type Definition def)
	   (:returns (type String)))
  (substitute #\- #\?			;#\? looses in tag
	      (format nil "Setf|~A"
		      (princ-with-nicknames (definition-symbol def)))))

(defmethod definition-class-nice-name ((def Setf-Definition))
  (declare (:returns "Setf"))
  "Setf")

(defmethod definition-name ((def Setf-Definition))
  "The name of a defsetf definition is a list like (setf foo)."
  (declare (type Setf-Definition def)
	   (:returns (type List)))
  `(setf ,(second (definition-form def))))

(defmethod definition-lambda-list ((def Setf-Definition))
  "Returns a lamdba list one would have for the equivalent setf
method, that is, new value first, followed by the lambda list for the
generalized variable."
  (declare (type Setf-Definition def)
	   (:returns (type List)))
  (let* ((form (definition-form def))
	 (3rd (third form)))
    (if (listp 3rd)
	;; then it's long format, report a lambda list like setf methods
	(cons (first (fourth form)) ;; store variable
	      (copy-list 3rd))
      ;; else short format, no arg list
      ())))

(defmethod definition-arg-types ((def Setf-Definition))
  "I haven't figured out a good way to get at the equivalent of arg
type declarations for defsetf, so this just returns nil."
  (declare (type Setf-Definition def)
	   (:returns nil))
  ())

(defmethod definition-documentation ((def Setf-Definition))

  "The doc string for defsetf is, in the long version of defsetf, the
5th item in the definition form (if it`s a string), and, in the short
version, the 4th item (again, if it's a string)."

  (declare (type Setf-Definition def)
	   (:returns (type String)))

  (let ((form (definition-form def)))
    (if (listp (third form))
	;; then it's long format
	(let ((5th (fifth form))) (if (stringp 5th) 5th ""))
      ;; else short format
      (let ((4th (fourth form))) (if (stringp 4th) 4th "")))))

(defmethod definition-declarations ((def Setf-Definition))
  "Declarations are only allowed in the long version of defsetf."
  (declare (type Setf-Definition def)
	   (:returns (type List)))
  (let ((form (definition-form def)))
    (if (listp (third form))
	;; then it's long format
	(let ((body (nthcdr 4 (definition-form def)))
	      (decls ()))
	  (loop
	    (let ((item (pop body)))
	      (cond ((stringp item) t)
		    ((and (listp item) (eql (first item) 'declare))
		     (setf decls (concatenate 'List (rest item) decls)))
		    (t (return)))))
	  decls)
      ;; else short format and no declarations allowed
      ())))

;;;-------------------------------------------------------

(defclass Generic-Function-Definition (Lambda-List-Definition) ()
  (:documentation
   "A definition class for <defgeneric>."))

(defmethod definition-class-nice-name ((def Generic-Function-Definition))
  (declare (:returns ))
  "Generic Function")

(defmethod definition-documentation ((def Generic-Function-Definition))
  
  "Return the empty string if no :documentation option is present."

  (declare (type Generic-Function-Definition def)
	   (:returns (type String)))

  (find-documentation-option (nthcdr 3 (definition-form def))))

;;;-------------------------------------------------------

(defclass Method-Definition (Lambda-List-Definition) ()
  (:documentation
   "A definition class for <defmethod>."))


(defmethod definition-class-nice-name ((def Method-Definition))
  (declare (:returns (type String)))
  (format nil "~:(~a~) Method"
	  (symbol-name (definition-method-qualifier def))))

(defmethod definition-name ((def Method-Definition))

  "The <definition-name> of a method is a list whose first item is the
symbol :method, whose second item is the function name, and whose
remaining items are the specializers for the required arguments."

  (declare (type Definition def)
	   (:returns (type List)))

  `(:method ,(second (definition-form def))
	    ,@(definition-specializers def)))

(defmethod definition-name->string ((def Method-Definition))

  "The name string for methods includes the specializers, so the
different methods for a generic function can be distinguished."
  (declare (type Method-Definition def)
	   (:returns (type String)))
  (let ((*print-case* :downcase)
	#+:excl (excl:*print-nickname* t))
    (format nil "~A~{ ~:(~A~)~}"
	    (princ-with-nicknames (definition-symbol def))
	    (mapcar #'princ-with-nicknames 
		    (definition-specializers def)))))

(defmethod definition-name->tag ((def Method-Definition))

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



(defmethod definition-lambda-list ((def Method-Definition))
  "Finding a method's lambda list requires checking for qualifiers."
  (declare (type Method-Definition def)
	   (:returns (type List)))
  (let ((3rd (third (definition-form def))))
    (if (atom 3rd)
	(fourth (definition-form def))
      3rd)))

(defmethod definition-body ((def Method-Definition))
  ;; Need to watch out for qualifiers
  (let ((3rd (third (definition-form def))))
    (nthcdr (if (atom 3rd) 4 3) (definition-form def))))

(defgeneric definition-specializers (def)
  (declare (type Method-Definition def)
	   (:returns (type List specializers)))
  (:documentation 
   "This function finds the list of specializers for a method
  function."))



(defmethod definition-specializers ((def Method-Definition))
  "Clos style definition, specialiers come from lambda-list"
  (lambda-list-specializers (definition-lambda-list def)))


#|
(defun definition-method-qualifier (def)

  "The method qualifier, eg., <after>. Returns <:primary> if no
qualifier present."

  (declare-check (type Method-Definition def)
		 (:returns (type Symbol)))

  (let ((3rd (third (definition-form def))))
    (if (atom 3rd) 3rd :primary)))
|#

(defun definition-method-qualifier (def)

  "The method qualifier, eg., <after>. Returns <:primary> if no
qualifier present."

  (declare (type Method-Definition def)
	   (:returns (type Symbol)))

  :primary)

  
