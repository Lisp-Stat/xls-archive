;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: :Definitions -*-
;;;  File Name          :  definition.lisp
;;;  Version Number     :  1.2
;;;  Last Changed Date  :  93/08/20 At 19:48:44
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

(defclass Definition ()
	  ((definition-form
	    :type List
	    :reader definition-form
	    :initarg :definition-form
	    :documentation
	    "The Lisp form that results from reading the definition.")
	   (definition-path
	    :type (or Pathname string)
	    :reader definition-path
	    :initarg :definition-path
	    :documentation
	    "Pathanme of the file from which the definition was read."))
  (:documentation
   "Abstract root class for definition objects."))
   
(defun definition-form (def)
	"CLOS style reader for definition-form"
	(send def 'definition-form))
(defun definition-path (def)
	"CLOS style reader for definition-form"
	(send def 'definition-path))


;;;-------------------------------------------------------

(defun definition-definer (def)
  "Returns the definer symbol, eg. <defun> or <defclass>."
  (declare (type Definition def)
	   (:returns (type Symbol)))
  (first (definition-form def)))

;;;-------------------------------------------------------

(defgeneric definition-definee (def)
  (declare (type Definition def)
	   (:returns (type T)))
  (:documentation
	     "Returns the lisp object that was created when the
definition was loaded (which is assumed to have happen before the
definition object was created), or nil if it is not possible to
retrieve a lisp object corresponding to the definition. For example,
one can get the appropriate class object by calling find-class on the
<definition-symbol>, but, there is in general no portable way a lisp
object associated with the result of evaluating a defstruct."))

(defmethod definition-definee ((def Definition))

  "The default method returns nil."

  (declare (type Definition def)
	   (:returns nil))
  nil)

;;;-------------------------------------------------------

(defgeneric definition-name (def)
	    (declare (type Definition def)
		     (:returns (type (or Symbol List))))
	    (:documentation
	     "Returns the name of a definition object, which is
usually either a symbol, eg. <foo> from (defun foo ...), (defclass Foo
...), etc., or a list, eg. (setf foo) from (defmethod (setf foo) ...)
or (defsetf foo ...)."))

(defmethod definition-name ((def Definition))

  "By default, the <definition-name> is the second item in the
definition-form."

  (declare (type Definition def)
	   (:returns (type (or Symbol List))))

  (second (definition-form def)))

;;;-------------------------------------------------------

(defgeneric definition-name->string (def)
	    (declare (type Definition def)
		     (:returns (type String)))
	    (:documentation
	     "Returns a string containing the name of a definition
object, appropriately capitalized."))

(defun princ-with-nicknames (name)
  "This function returns a string for a symbol.  It uses the package
nickname if available."
  (let* ((pack (symbol-package name))
	 (packname (cond ((keywordp name) "")
			 ((package-nicknames pack)
			  (car (package-nicknames pack)))
			 (t (package-name pack)))))
    (format nil "~(~A~:[::~;:~]~A~)"
	    packname (exported-symbol? name) (symbol-name
					      name))))



(defmethod definition-name->string ((def Definition))

  "The default method simply calls format on the <definition-name>,
printing in lower case."

  (declare (type Definition def)
	   (:returns (type String)))
  (let ((name (definition-name def)))
    (if (symbolp name)
	;; standard type def
	(princ-with-nicknames name)
      ;; (setf name) style
      (let ((sym (definition-symbol def))
	    (def-acc (car name)))
	(format nil "~(~S ~A~)"
		def-acc
		(princ-with-nicknames sym)))
      )))

    
;;;------------------------------------------------------

(defgeneric definition-name->tag (def)
	    (declare (type Definition def)
		     (:returns (type String)))
	    (:documentation
	     "Returns a tag containing the name of a definition
object, suitable for use with HTML tagging."))

(defmethod definition-name->tag ((def Definition))

  "The default method simply calls format on the <definition-name>,
printing in lower case."

  (declare (type Definition def)
	   (:returns (type String)))
  (substitute #\- #\?			;#\? looses in tag
	      (format nil "Def|~A" (definition-name->string def))))


;;;-------------------------------------------------------

(defgeneric definition-symbol (def)
	    (declare (type Definition def)
		     (:returns (type Symbol)))
	    (:documentation
	     "Returns a symbol naming the definition.  For definitions
whose <definition-name> is a symbol, <definition-symbol> is the same.
For definitions whose <definition-name> is a list like (setf foo),
<definition-symbol> is <foo>."))

(defmethod definition-symbol ((def Definition))

  "Returns a symbol naming the definition.  For definitions whose
<definition-name> is a symbol, <definition-symbol> is the same.  For
definitions whose <definition-name> is a list like (setf foo),
<definition-symbol> is <foo>."

  (declare (type Definition def)
	   (:returns (type Symbol)))
  (let ((name (definition-name def)))
    (if (listp name)
	(if (listp (second name))
	    (second (second name))
	  (second name))
      name)))

(defun definition-symbol-name (def)
  "Returns the symbol-name of the <definition-symbol>."
  (declare (type Definition def)
	   (:returns (type String)))
  (symbol-name (definition-symbol def)))

;;;-------------------------------------------------------

(defgeneric definition-usage (def)
	    (declare (type Definition def)
		     (:returns (values (type String usage)
				       (type Fixnum indent))))
	    (:documentation
	     "Returns a string showing how to ``call'' the definition.
              Second value is indentation to use if it goes to
              multiple lines."))

(defmethod definition-usage ((def Definition))
  "The default for usage is just the <definition-name-string>."
  (declare (type Definition def)
	   (:returns (values (type String usage)
			     (type Fixnum indent))))
  (values (definition-name->string def) 0))

;;;-------------------------------------------------------

(defgeneric definition-lambda-list (def)
	    (declare (type Definition def)
		     (:returns (type List)))
	    (:documentation
	     "Returns an arglist for the definition, or nil."))

(defmethod definition-lambda-list ((def Definition))
  "The default method returns ()."
  (declare (type Definition def)
	   (:returns ()))
  ())

;;;-------------------------------------------------------

(defgeneric definition-arg-types (def)
	    (declare (type Definition def)
		     (:returns (type List)))
	    (:documentation
	     "Returns a list of arg --- type pairs for the definition."))

(defmethod definition-arg-types ((def Definition))
  "The default method returns ()."
  (declare (type Definition def)
	   (:returns ()))
  ())

;;;-------------------------------------------------------

(defgeneric definition-documentation (def)
  (declare (type Definition def)
	   (:returns (type String)))
  (:documentation
   "Return the documentation string associated with <def>.
Return a string of length zero if there is no documentation string."))

(defmethod definition-documentation ((def Definition))

  "The default is the 4th item in the definition form, if it's a
string, otherwise we return the empty string."

  (declare (type Definition def)
	   (:returns (type String)))
  (let ((4th (fourth (definition-form def))))
    (if (stringp 4th) 4th "")))

;;;-------------------------------------------------------

(defgeneric definition-declarations (def)
  (declare (type Definition def)
	   (:returns (type List)))
  (:documentation
   "Returns all the decl-specs associated with the entire definition.
A decl-spec is a list like (type Fixnum x).  By associated with the
entire definition, we mean, for example, the declarations with scope
over an entire function body, excluding those local to a particular
<let>. At present, global declarations (from <proclaim> or <declaim>)
are ignored."))

(defmethod definition-declarations ((def Definition))
  "The default method returns ()."
   (declare (type Definition def)
	   (:returns ()))
   ())

(defun definition-type-declarations (def)

  "Returns a list of the type decl specs. At the moment, a type decl
spec must have the symbol <type> as it's first item.  In the future
this may be extended to cover decl specs whose first entry is, for
example, <Fixnum>."

  (declare (type Definition def)
	   (:returns (type List)))

  (remove-if-not #'(lambda (decl) (eq 'type (first decl)))
		 (definition-declarations def)))

(defun definition-returns (def)
  "Returns the :returns decl-spec or nil if there isn't one."
  (declare (type Definition def)
	   (:returns (type List)))
  (find-if #'(lambda (decl) (member (first decl) '(:returns)))
	   (definition-declarations def)))

;;;-------------------------------------------------------

(defgeneric definition-slots (def)
	    (declare (type Definition def)
		     (:returns (type List)))
	    (:documentation
	     "Returns a list of slot specs, which need to be
interpreted in a Definition class specific manner (structure slot
specs are different from class slot specs)."))

(defmethod definition-slots ((def Definition))
  "The default method returns ()."
  (declare (type Definition def)
	   (:returns ()))
  ())

;;;-------------------------------------------------------

(defgeneric definition-parents (def)
	    (declare (type Definition def)
		     (:returns (type List)))
	    (:documentation
	     "Returns a list of the names of the parents (eg. direct
superclasses) of the definition.  For new Definition classes, it may
return anything that is reasonably thought of as a name of a
``parent'' of the definition."))

(defmethod definition-parents ((def Definition))
  "The default method returns ()."
  (declare (type Definition def)
	   (:returns ()))
  ())

;;;-------------------------------------------------------

(defgeneric definition-children (def &optional package)
	    (declare (type Definition def)
		     (type (or Symbol String Package) package)
		     (:returns (type List)))
	    (:documentation
	     "Returns a list of names of children (eg. direct
subclasses) of the definition.  For new Definition classes, it may
return anything that is reasonably thought of as a name of a ``child''
of the definition.  
  If package is non-nil, will only search for definitions in a given
(exported) package."))

(defmethod definition-children ((def Definition) &optional package)
  "The default method returns ()."
  (declare (type Definition def)
	   (:returns ()))
  ())

(defgeneric definition-methods (def &key all)
	    (declare (type Definition def)
		     (type (or T Nil) all)
		     (:returns (type List)))
	    (:documentation
	     "Returns a list of names of methods associated with the
definee.  For new Definition classes, it may 
return anything that is reasonably thought of as a name of a ``message''
of the definition.  
  If all is true, it will list inherited as well as local messages."))

(defmethod definition-methods ((def Definition) &key all)
  "The default method returns ()."
  (declare (type Definition def)
	   (type (or T Nil) all)
	   (:returns ()))
  ())


;;;===========================================================

;;;=======================================================
;;; Nice definition names

(defgeneric definition-class-nice-name (def)
	    (declare (type Definition def)
		     (:returns (type String)))
	    (:documentation
	     "Returns a short string for the type of the definition,
eg. ``Class'' for <Class-Definition>."))

(defmethod definition-class-nice-name ((def Definition))
  (declare (:returns "Unknown-Definition-Type"))
  "Unknown-Definition-Type")



