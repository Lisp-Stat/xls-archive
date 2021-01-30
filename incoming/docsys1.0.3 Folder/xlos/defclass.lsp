;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: :XLOS -*-

;;;  File Name          :  defclass.lisp
;;;  Version Number     :  1.0
;;;  Last Changed Date  :  6/9/98
;;;  Created Date       :  6/9/98
;;;
;;; Copyright 1998. Russell G. Almond.
;;;
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
  (:export "SLOT-TYPES" "DEFCLASS"
	   "INITIALIZE-SLOT" "PROCESS-INITARGS"
	   "MAKE-INSTANCE" 
	   "OBJTYPEP" "MYTYPEP" "MY-CHECK-TYPE"
	   "SHARED-INITIALIZE"
	   ))

(in-package :xlos)

;;; Caveat Emptor.  This is a quick and dirty program meant to (a)
;;; give a more declarative syntax to the creation of XLS objects,
;;; particularly give a syntax which supports the automatic
;;; documentation, type checking and the creation of accessor
;;; methdods. (b) provide a quick translation method of CLOS code to
;;; XLS code.  It is not fully CLOS compliant and you should expect
;;; several differences.

;;; 1. Behavior of shared slots.  Under XLS a shared slot may only be
;;; set by the owning object, not by an instance.  The system allows
;;; the creation of an accessor method which sets the slot, but it
;;; will generate an error unless it is called on the prototype.

;;; 2. Behavior of the :default-initargs does not comply with
;;; Steele[1990] 28.1.9.5.  In particular, initargs for the instance
;;; will override values of the initargs supplied for the same slot by
;;; the prototype.  Precidence within the the initargs for the
;;; instance will find the first matching initarg in the list.
;;; Implementers would do well to not rely on precidence among
;;; multiple initargs/default-initargs, nor to rely on side effects
;;; when evaluating initforms or default-initargs.

;;; Slot type system.  XLS doesn't currently have any way to specify
;;; or retrieve slot types.  We will fix this by adding a slot
;;; xlos:slot-types to the *object* and creating a method :slot-type
;;; which fetches it for a given type.

(defconstant XLOS:SLOT-TYPES 'XLOS:SLOT-TYPES
  "The name of a slot for storing type information.")

(send *object* :add-slot 'XLOS:SLOT-TYPES nil)

(defmeth *object* :slot-type (slot-name &optional type-specifier)
  "Gets/Sets the type-specifier for the slot named <slot-name>.
Results are undefined in <type-specifier> is not a legal type
specifier.  This type specifier is only check when setting a slot
through a constructor made by the defclass system."
  (declare (type Symbol slot-name) (type (or T Null) type-specifier)
	   (:returns (type T type-specifier)))
  (block find-type
    (if type-specifier
	;; This is a set operation.
	(let* ((type-specs (slot-value 'XLOS:SLOT-TYPES))
	       (our-spec (assoc slot-name type-specs)))
	  (if our-spec
	      ;;Changing old spec
	      (setf (cdr our-spec) type-specifier)
	    ;; Adding new value
	    (push (cons slot-name type-specifier) 
		  (slot-value 'XLOS:SLOT-TYPES)))
	  type-specifier)  ; for consistency, return type specifier.
      ;;This is a get operation.
      (let* ((type-specs (slot-value 'XLOS:SLOT-TYPES))
	     (our-spec (assoc slot-name type-specs)))
	(if our-spec (return-from find-type (cdr our-spec))) 
					; found it in the object
					; itself.
	;; Else, search precidence list
	(dolist (proto (send self :precedence-list))
	  (setf type-specs (send proto :slot-value 'XLOS:SLOT-TYPES))
	  (setf our-spec (assoc slot-name type-specs))
	  (if our-spec (return-from find-type (cdr our-spec))) ;found it
	  )
	;; No type specified, type must be T
	(return-from find-type T)))))


;;; Metaobject definitions for xlos.  There are two meta-object, a
;;; slot metaobject and a class metaobject.  These are transient
;;; objects which go away once the appropriate prototype and other
;;; forms are built.


(defstruct xlos-slot
;  "This is a CLOS style slot description.  It is used in parsing the
;CLOS form to produce a corresponding XLISP form."
  (name nil) ;name of the slot
  (reader nil) ;Name of the reader function
  (writer nil) ;name of the writer function
  (accessor nil) ;name of the accessor function
  (allocation :default) ;type of allocation
			 ; should be :instance, :prototype or :class
  (initarg nil) ; name of keyword arg for creationg function
  (initform nil) ; default value for slot
  (initform-supplied-p nil) ; was an intiform value supplied yet?
  (type T) ; type specificier for slot content
  (documentation ""); string indicating type of object
  )

(defstruct xlos-class
;  "This is a class description metaobject, created in the process of
;building parsing a defclass form."
  (name nil) ;name of the class
  (superclasses '(*object*)) ; list of prototypes
  (metaclass *object*) ; MOP stuff, probably unused
  (documentation "") ; Doc string for object.
  (default-initargs '()) ; list of default values for initargs.
  (slots '()) ; list of slot specifiers
  )

;;; This macro is the heart of the defclass system.  It takes a
;;; CLOS-syntax defclass form and creates a series of XLISP-STAT forms
;;; for creating an appropriate prototype and methods.


(defmacro defclass (name prototypes slot-specifications 
			 &rest class-options)
  "This macro provides the defclass syntax for the XLISP-STAT object
system.  It creates an appropriate defproto form, along with
appropriate creation (:isnew) and accessor methods.  For more
information, see Steele [1990], p 822.

    Usage
    defclass class-name ({superclass-name}*)({slot-specifier}*)[class-
      option]
    class-name:: symbol
    superclass-name:: symbol
    slot-specifier:: slot-name | (slot-name [slot-option])
    slot-name:: symbol
    slot-option::
     {:reader reader-function-name}* |
     {:writer writer-function-name}* |
     {:accessor reader-function-name}* |
     {:allocation allocation-type} |
     {:initarg initarg-name}* |
     {:initform form} |
     {:type type-specifier} |
     {:documentation string}
    reader-function-name:: symbol
    writer-function-name:: function-specifier
    function-specifier:: {symbol }
    initarg-name:: symbol
    allocation-type:: {:instance | :class | :prototype}
    class-option::
     (:default-initargs initarg-list) |
     (:documentation string) 
    initarg-list:: {initarg-name default-initial-value-form}*
"
  (let ((class-mo (make-xlos-class :name name 
				   :superclasses prototypes))
	(form nil))			;This will be used to
					;accumulate the output.
    (parse-slots class-mo slot-specifications)
    (parse-options class-mo class-options)
    ;; now write out a series of XLS forms which will do the work.
    ;; we will push these onto the variable form in reverse order.
    (dolist (slot-mo (xlos-class-slots class-mo))
      ;; this function creates readers writers and slot
      ;; documentation for each slot.
      (setf form 
	    (nconc (add-accessors class-mo slot-mo)
		   form)))
    ;; Add a default constructor
    (push (add-constructor class-mo) form)
    ;; Add a special initialization form for the type-spec system.
    (push (add-typespec-kludge class-mo) form)
    ;; Finally, add the defproto macro
    (push (add-defproto class-mo) form)
    `(progn ,.form)))


;;; The following functions are helper functions for parsing part of
;;; the defclass form into the class and slot metaobjects

;;; Slot metaobjects

(defun parse-slots (class-mo slot-specifiers)
  (declare (type xlos-class class-mo) (type list slot-specifers)
	   (:returns (type xlos-class class-mo)))
  "This function parses the slot-specifiers and destructively modifies
its class-mo argument to contain a list of slot specifications."
  (if (endp slot-specifiers) class-mo
    (progn
      (push (parse-a-slot (car slot-specifiers))
	    (xlos-class-slots class-mo))
      (parse-slots class-mo (cdr slot-specifiers)))))
	    
      
(defun parse-a-slot (slot-specifier)
  (declare (type (or symbol list slot-specifier))
	   (:returns (type xlos-slot slot-mo)))
  "This function parses a single slot and creates a corresponding slot
metaobject."
  (cond ((null slot-specifier)
	 (error "Expecting a slot name or specification list, got nil"))
	((symbolp slot-specifier)
	 (make-xlos-slot :name slot-specifier))
	((listp slot-specifier)
	 (let* ((name (car slot-specifier))
		(slot-mo (make-xlos-slot :name name)))
	   (parse-slot-options slot-mo (cdr slot-specifier))
	   slot-mo))
	(t (error "Expecting a slot name or specification, got ~S" 
		  slot-specifier))))

(defun parse-slot-options (slot-mo slot-option-list)
  (declare (type xlos-slot slot-mo) (type list slot-option-list)
	   (:returns (type xlos-slot slot-mo)))
  "This function parse the options for a slot list."
  (if (null slot-option-list) slot-mo
    (if (< (length slot-option-list) 2)
	(error "Odd number of elements in option list for slot ~S"
	       (xlos-slot-name slot-mo))
      (let ((option-name (car slot-option-list))
	    (option-value (cadr slot-option-list)))
	(case option-name
	  (:reader (push option-value (xlos-slot-reader slot-mo)))
	  (:writer (push option-value (xlos-slot-writer slot-mo)))
	  (:accessor (push option-value (xlos-slot-accessor slot-mo)))
	  (:allocation 
	   (if (member option-value '(:instance :class :prototype))
	       (if (eql (xlos-slot-allocation slot-mo) :default)
		   (setf (xlos-slot-allocation slot-mo) option-value)
		 (error "Keyword :allocation used more than once."))
	     (error "Unknown value for :allocation keyword, ~S" 
		    option-value)))
	  (:initarg (push option-value (xlos-slot-initarg slot-mo)))
	  (:initform
	   (if (xlos-slot-initform-supplied-p slot-mo)
	       (error "More than one :initform supplied for slot.")
	     (progn 
	       (setf (xlos-slot-initform-supplied-p slot-mo) t)
	       (setf (xlos-slot-initform slot-mo) option-value)))
	     )
	  (:type 
	   (if (eql (xlos-slot-type slot-mo) T)
	       (setf (xlos-slot-type slot-mo) option-value)
	     (error "More than one :type specifier for slot ~S" 
		    (xlos-slot-name slot-mo))))
	  (:documentation 
	   (if (equal (xlos-slot-documentation slot-mo) "")
	       (setf (xlos-slot-documentation slot-mo) option-value)
	     (error "More than one :documentation specifier for slot ~S" 
		    (xlos-slot-name slot-mo))))
	  (t (error "Unrecognized slot option, ~S" option-name))
	  )
	(parse-slot-options slot-mo (cddr slot-option-list))
	))))

		

;;; class metaobjects
  
(defun parse-options (class-mo class-options)
  (declare (type xlos-class class-mo) (type list class-options)
	   (:returns (type xlos-class class-mo)))
  "Parses the class option forms of the defclass statement."
  (if (null class-options) class-mo
    (let* ((option-form (car class-options))
	   (option-name (car option-form)))
      (case option-name
	(:default-initargs 
	 (if (eql (xlos-class-default-initargs class-mo) nil)
	     (let ((option-value (cdr option-form)))
	       (if (or (not (listp option-value))
		       (oddp (length option-value)))
		   (error ":Default-initargs must be a list of even length.")
		 (setf (xlos-class-default-initargs class-mo) option-value)))
	   (error "More than one :default-initargs specifier for class ~S"
		  (xlos-class-name class-mo))))
	(:documentation 
	 (if (equal (xlos-class-documentation class-mo) "")
	     (setf (xlos-class-documentation class-mo) (cadr option-form))
	   (error "More than one :documentation specifier for class ~S"
		  (xlos-class-name class-mo))))
	(:metaclass 
	 (if (not (eql option-value *object*))
	     (error "Only know how to make classes with metaobject ~S"
		    *object*)))
	(t (error "Unknown class option ~S for class ~S" option-name
		  (xlos-class-name class-mo))))
      (parse-options class-mo (cdr class-options)))))

;;; The following functions build the specific forms which are chained
;;; together to make a complete defclass specification.

(defun add-defproto (class-mo)
  (declare (type xlos-class class-mo) 
	   (:returns (type list form)))
  "Creates a defproto form object."
  (let ((instance-slots nil) (shared-slots nil))
    ; build up the list of instance and shared slots
    (dolist (slot-mo (xlos-class-slots class-mo))
      (case (xlos-slot-allocation slot-mo)
	((:default :instance) 
	 (push (xlos-slot-name slot-mo) instance-slots))
	((:class :prototype :shared)
	 (push (xlos-slot-name slot-mo) shared-slots))
	(t (error "Unknown allocation type ~S" 
		  (xlos-slot-allocation slot-mo)))))
    `(defproto ,(xlos-class-name class-mo) 
       ',instance-slots
       ',shared-slots
       (list ,@(xlos-class-superclasses class-mo))
       ,(xlos-class-documentation class-mo))
    ))


;;; Constructor relies on the xlos:shared-initialize method.
(defconstant SHARED-INITIALIZE 'SHARED-INITIALIZE
  "The name for the shared-initialize method.")

(defun shared-initialize (obj &rest keyword-args)
  (declare (type OBJECT obj))
  "The shared-initialize generic function takes care of initializing
slot values.  It takes the keyword part of the initialization lambda
list and sets up the initial values for the slots to either (a) the
first appropriate keyword it finds or else (b) it executes the
initform for the slot."
  (apply #'send obj SHARED-INITIALIZE keyword-args))

(defun add-constructor (class-mo)
  (declare (type xlos-class class-mo) 
	   (:returns (type list form)))
  "Creates a form for a default :isnew method for object."
  (let ((arg-processors '(self))) ; :isnew returns self
    (dolist (slot-mo (xlos-class-slots class-mo))
      (when (or (xlos-slot-initarg slot-mo)
		(xlos-slot-initform-supplied-p slot-mo)) 
	(push (add-arg-processor class-mo slot-mo) arg-processors)))
    `(progn
       (defmeth ,(xlos-class-name class-mo) SHARED-INITIALIZE
	 (&rest args)
	 ,.arg-processors)
       (defmeth ,(xlos-class-name class-mo) :isnew
	 (&rest args)
	 (setf args
	   (xlos:process-initargs ',(xlos-class-default-initargs class-mo)
				   args))
	 (apply #'call-next-method args)
	 (apply #'xlos:shared-initialize self args)
	 self)
       )
    ))

(defun add-arg-processor (class-mo slot-mo)
  (declare (type xlos-class class-mo) (type xlos-slot slot-mo)
	   (:returns (type list form)))
  "This creates a form which looks for the appropriate initarg in the
argument list and sets the slot to the appropriate value if found."
  `(xlos::initialize-slot self ',(xlos-slot-name slot-mo) 
			  ',(xlos-slot-initarg slot-mo) args
			  ',(xlos-slot-initform slot-mo))
  )





;;; These three functions are auxilary functions called by the default
;;; :isnew method 

(defun initialize-slot (obj slot-name initargs args initform)
  "This function does the work of initializing a slot.  It searches,
the list of args for the first one matching initargs.  If found, it
sets the slot to that value.  If not, it evaluates initform and sets
the slot to that value. 
  If the initial value does not match the type of the slot, it signals
an error."
  (let ((value (find-value-in-arglist initargs args initform)))
    (unless (mytypep value (send obj :slot-type slot-name))
      (error "Expected a value of type ~S, got ~S"
	     (send obj :slot-type slot-name) value))
    (setf (slot-value slot-name) value)))

(defun find-value-in-arglist (initargs args initform)
  "This function searches for an argument whose name appears on the
initargs list in the args (which should be a list of keyword
arguments.  If it doesn't find one, it evaluates the <initform>
expression to get a default value to return."
  (if (null args) (eval initform)
    (if (< (length args) 2)
	(error "Odd number of arguments in keyword list.")
      (if (member (car args) initargs)
	  ;; Found it, return this one
	  (cadr args)
	(find-value-in-arglist initargs (cddr args) initform)))))


(defun process-initargs (default-initargs actual-args)
  (declare (type list default-initargs) (type list actual-args)
	   (:returns (type list actual-args)))
  "This function does the processing for the default initargs.  It
searches through the list of initargs and extracts the ones which are
not already on the list and appends them to the end."
  (if (null default-initargs) actual-args
    (let ((initarg (car default-initargs))
	  (initform (cadr default-initargs)))
      (unless (find initarg actual-args)
	;; Arg not found, append default value to end of arglist
	(setf actual-args
	      (append actual-args (list initarg (eval initform)))))
      (process-initargs (cddr default-initargs) actual-args))))


(defun add-typespec-kludge (class-mo)
  (declare (type xlos-class class-mo) 
	   (:returns (type list form)))
  "Creates a form to clear the typespec information, so that we
inherit it for inherited slots rather than copying it down."
  `(send ,(xlos-class-name class-mo) :add-slot XLOS:SLOT-TYPES nil)
  )


(defun add-accessors (class-mo slot-mo)
  (declare (type xlos-class class-mo) (type xlos-slot slot-mo)
	   (:returns (type list form-list)))
  "Creates a list of forms recreating readers, writers accessors and
documentation for a given slot."
  (let ((form-list '()))
    (dolist (reader (xlos-slot-reader slot-mo))
      (push (add-reader class-mo slot-mo reader) form-list))
    (dolist (writer (xlos-slot-writer slot-mo))
      (push (add-writer class-mo slot-mo writer) form-list))
    (dolist (accessor (xlos-slot-accessor slot-mo))
      (push (add-accessor class-mo slot-mo accessor) form-list))
    (unless (eql (xlos-slot-type slot-mo) t)
      (push (add-slot-type class-mo slot-mo) form-list))
    (unless (equal (xlos-slot-documentation slot-mo) "")
      (push (add-slot-documentation class-mo slot-mo) form-list))
    form-list
    ))

(defun add-slot-documentation (class-mo slot-mo)
  (declare (type xlos-class class-mo) (type xlos-slot slot-mo)
	   (:returns (type list form)))
  "Creates a slot documenationform for the specified slot."
  `(send ,(xlos-class-name class-mo) :documentation 
	 ',(xlos-slot-name slot-mo) 
	 ,(xlos-slot-documentation slot-mo))
  )

(defun add-slot-type (class-mo slot-mo)
  (declare (type xlos-class class-mo) (type xlos-slot slot-mo)
	   (:returns (type list form)))
  "Creates a slot type form for the specified slot."
  `(send ,(xlos-class-name class-mo) :slot-type
	 ',(xlos-slot-name slot-mo) 
	 ',(xlos-slot-type slot-mo))
  )


(defun add-reader (class-mo slot-mo reader)
  (declare (type xlos-class class-mo) (type xlos-slot slot-mo)
	   (type (or Keyword Symbol T) reader)
	   (:returns (type list form)))
  "Creates a defmeth form for a reader for the specified slot."
  `(defmeth ,(xlos-class-name class-mo) ,reader ()
     ,(format nil "Reader for slot ~S [~S]~%~A" 
	      (xlos-slot-name slot-mo) (xlos-slot-type slot-mo)
	      (xlos-slot-documentation slot-mo))
     (declare (:returns (type ,(xlos-slot-type slot-mo) value)))
     (slot-value ',(xlos-slot-name slot-mo)))
  )

(defun add-writer (class-mo slot-mo writer)
  (declare (type xlos-class class-mo) (type xlos-slot slot-mo)
	   (type (or Keyword Symbol T) writer)
	   (:returns (type list form)))
  "Creates a defmeth form for a writer for the specified slot."
  `(defmeth ,(xlos-class-name class-mo) ,writer (value)
     ,(format nil "Writer for slot ~S [~S]~%~A" 
	      (xlos-slot-name slot-mo) (xlos-slot-type slot-mo)
	      (xlos-slot-documentation slot-mo))
     (declare (type ,(xlos-slot-type slot-mo) value))
     (let ((type (send ,(xlos-class-name class-mo) :slot-type
			',(xlos-slot-name slot-mo))))
       (if (not (mytypep value type))
	 (error "Expected type ~S, got ~S" type value)))
     (setf (slot-value ',(xlos-slot-name slot-mo)) value))
  )

(defun add-accessor (class-mo slot-mo accessor)
  (declare (type xlos-class class-mo) (type xlos-slot slot-mo)
	   (type (or Keyword Symbol T) accessor)
	   (:returns (type list form)))
  "Creates a defmeth form for a accessor for the specified slot."
  `(defmeth ,(xlos-class-name class-mo) ,accessor 
     (&optional (value nil supplied-p))
     ,(format nil "Accessor for slot ~S [~S]~%~A" 
	      (xlos-slot-name slot-mo) (xlos-slot-type slot-mo)
	      (xlos-slot-documentation slot-mo))
     (declare (type (or NULL ,(xlos-slot-type slot-mo)) value))
     (if supplied-p
	 ;; Setting the value
	 (let ((type (send ,(xlos-class-name class-mo) :slot-type
			   ',(xlos-slot-name slot-mo))))
	   (if (not (mytypep value type))
	       (error "Expected type ~S, got ~S" type value))
	   (setf (slot-value ',(xlos-slot-name slot-mo)) value))
       ;; Retrieve the value.
       (slot-value ',(xlos-slot-name slot-mo))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workaround for the fact that XLS objects are not types.

(defun objtypep (val obj-name)
  "Tests to see if val is an XLS object which is an instance of <obj-name>."
  (declare (type T val) (type Symbol obj-name)
	   (:returns (type (member '(T Nil)) )))
  (if (and (typep val 'OBJECT) 
	   (symbolp obj-name)
	   (boundp obj-name))
      (kind-of-p val (symbol-value obj-name))
    nil))


(defun mytypep (val typ)
  "This function performs the same as typep, only if the second
  argument is the name (symbol) of a prototype, then it check to see
  if val is an object of the appropriate kind."
  (declare (type T val) (type T typ)
	   (:returns (type (member '(T Nil)) )))
  (cond ((typep val typ) T)
	((and (typep val 'OBJECT) (symbolp typ))
	 (objtypep val typ))
	(t nil)))

(defmacro my-check-type (place spec &optional string)
  "This is a version of the check-type macro which will accept the
  name (symbol) of an object as a type name."
  (let ((valsym (gensym "VAL")))
    `(loop
      (let ((,valsym ,place))
	(if (mytypep ,valsym ',spec)
	    (return nil)
	    (restart-case
	     (error 'check-type-error
		    :datum ,valsym
		    :expected-type ',spec
		    :form ',place
		    :type-string ,string)
	     (store-value (,valsym)
	      :report "Store new value."
	      :interactive (lambda ()
			     (list
			      (prompt-for ',spec "Value for ~s: " ',place)))
	      (setf ,place ,valsym))))))))


;;; This is necessary because there is no pathname type defined in
;;; XLISP.  Pathnames are implemented as strings and so (typep
;;; "foo.lsp" 'pathname) should return true.

(deftype lisp::pathname () '(satisfies stringp))



;;;;;;;;;;;;;;;;;;;;
;;; Quick and dirty make-instance

(defun make-instance (obj &rest initargs)
  "This function is a quick and dirty implementation of make-instance.  It 
  expects either an XLS object or a symbol whose value is an XLS object.  
  It sends the object a :new method with args <initargs> and returns the 
  result."
  (declare (type (or Symbol Object) obj)
	   (:returns (type obj new-obj)))
  (if (symbolp obj) (setq obj (symbol-value obj)))
  (apply #'send obj :new initargs))
  



