;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines child mix-ins and update functions.

(require :el-utils (strcat ElToY-directory "utils" *d-sep*  "utils.lsp"))


;;; **********************************************************************
;;; Update extensions to ElToY-object
;;; **********************************************************************
(defmeth ElToY-object :update (signal &rest data)
  (when (find :update (trace))
	(format t "Object: ~S~%" self))
  (when (send self :has-method signal)
	(if (find :update (trace))
	    (format t ":update (ElToY-object) sending local signal ~S~%"
		    signal))
	(apply #'send  self signal :local data)))

(send ElToY-object :documentation :update
  ":update <signal> (&rest data) --- if object has a method for signal
   it will send itself the message: <signal> :local data

:update logic:  Child objects send change messages to parent objects.
Parent objects, if they own the object to be changed, turn non-local
messages (change messages not preceded by :local) to :update requests.
The :update method generates a :local request (if an appropriate
method exists) and then sends update messages to all child objects."
)




;
;;;**********************************************************************
;;; subtool-mixin --- mixed in if object is a child of a supertool
;;;**********************************************************************

(defproto subtool-mixin
  '(supertool)				;parent ElToY-object
  '() (list ElToY-object)
  "Add a parent to an object, handle paranet update requests")


(defmeth subtool-mixin :isnew (&rest args
				&key (supertool nil)
				&allow-other-keys)
  (when (null supertool)
	(error "~S is a subtool, must have supertool" self))
  (setf (slot-value 'supertool) supertool)
  (apply #'call-next-method args))

(send subtool-mixin :documentation :isnew
      ":supertool --- sets ElToY-parent object")

;; :describe
(defmeth subtool-mixin :describe (&optional (stream t) (verbose nil))
  (format stream "Supertool: ~S~%" (send self :parent))
  (if verbose (send (send self :supertool) :describe stream nil))
  (call-next-method stream verbose))



;; :supertool
(defmeth subtool-mixin :supertool (&rest args)
  (if args (apply #'send (slot-value 'supertool) args)
    (slot-value 'supertool)))

(send subtool-mixin :documentation :supertool
      "Method args: (&rest args)
Retrieves ElToY parent object.  If args, sends args as a message to
parent.")

(defmeth subtool-mixin :set-supertool (&optional (parent nil set))
  (if set (setf (slot-value 'supertool) parent)
    (slot-value 'supertool)))

(send subtool-mixin :documentation :set-supertool
      "Method args: (&optional parent)
Returns/sets ElToY parent object")





;
;;;**********************************************************************
;;; Mirror-parent --- A parent object for creating a perfectly
;;;                   reflective mixin
;;;**********************************************************************

;;; mirror-parent  This object is meant to be a perfectly reflecting
;;; mirror for any tool which is normally a child

(defproto mirror-parent-proto '(subtool) '()
  (list ElToY-object)
"Perfectly reflective top-level object"
)

;; :isnew --- takes one argument which is a function with a singal
;; argument self.  This function when called, should return  the child
;; object.  It calls next-method with remaining args first, so mixins
;; (like parameters) should be set.

(defmeth mirror-parent-proto :isnew (make-child &rest args)
  (prog1 
      (apply #'call-next-method args)
    (setf (slot-value 'subtool)
	  (apply make-child (list self)))))

(send mirror-parent-proto :documentation :isnew
      (strcat
       "Mirror-parent-proto :make-child  (lambda (self)) ---  The
        make-child argument should return a function of one argument
        (the parent object) which when called will create the child
        object, returning it.

"
       (send ElToY-object :documentation :isnew)
       "Note: calls next-method first"))



(defmeth mirror-parent-proto :destruct (&rest args)
  (send (slot-value 'subtool) :destruct)
  (setf (slot-value 'subtool) nil)
  (apply #'call-next-method args))


(defmeth mirror-parent-proto :update (signal &rest data)
  (apply #'call-next-method signal data)
  (when (slot-value 'subtool)
	(if (find :update (trace))
	    (format t ":update (mirror-parent-proto) send to subtool~%"))
	(apply #'send (slot-value 'subtool) :update signal data)))


(send mirror-parent-proto :documentation :update
      (strcat
       ":update <signal> (&rest data) --- after sending :local signal
        sends :update <signal> data to subtool.
"
      (send ElToY-object :documentation :update)))


;;;************************************************************************
;;;ElToY-dialog-proto
;;;************************************************************************

;;; dialog object conforming to certain ElToY standards

(defproto ElToY-dialog-proto '() '()
  (list dialog-proto ElToY-object))

;; :destruct
(defmeth ElToY-dialog-proto :destruct (&rest args)
  (send self :dispose))

(defmeth ElToY-dialog-proto :update (signal &rest data)
  (when (find :update (trace))
	(format t "Object: ~S~%" self))
  (when (send self :has-method signal)
	(if (find :update (trace))
	    (format t ":update (ElToY-object) sending local signal ~S~%"
		    signal))
	(apply #'send  self signal :local data)))


(send ElToY-dialog-proto :documentation :update
  ":update <signal> (&rest data) --- if object has a method for signal
   it will send itself the message: <signal> :local data

:update logic:  Child objects send change messages to parent objects.
Parent objects, if they own the object to be changed, turn non-local
messages (change messages not preceded by :local) to :update requests.
The :update method generates a :local request (if an appropriate
method exists) and then sends update messages to all child objects."
)


(new-provide :el-update)
