;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines stupid utility mix-ins, frequently used.

(require :new-provide (strcat ElToY-directory "utils" *d-sep*  "new-provide.lsp"))
;(require "menubar" (strcat ElToY-directory "menubar.lsp"))
;(require :el-xlispstatx (strcat ElToY-directory "utils" *d-sep*  "xlispstatx.lsp"))
(require :el-superset (strcat ElToY-directory "utils" *d-sep*  "superset.lsp"))
(require :el-checks (strcat ElToY-directory "utils" *d-sep*  "checks.lsp"))



;;; ElToY-object --- an ancestor of all ElToY objects, for defining
;;; catchall universal methods.
(defproto ElToY-object  '() '() '()
  "Is an El-ToY object")


;; :isnew
(defmeth ElToY-object :isnew (&rest args)
  self)

(send ElToY-object :documentation :isnew 
      "All ElToY objects return self when created")

;; :describe
(defmeth ElToY-object :describe (&optional (stream t) (verbose t))
  (if verbose 
      (format stream "~S is an ElToY-object~%" self)))

(send ElToY-object :documentation :describe
      "Method args: (&optional (stream t) (verbose t))
All ElToY objects describe <self>")

;; :destruct
(defmeth ElToY-object :destruct (&rest args)
  nil)

(send ElToY-object :documentation :destruct 
      "Destroy with proper cleanup")

;
;;;**********************************************************************
;;; titled-object  --- objects with a title
;;;**********************************************************************

(defproto titled-object 
  '(title)				;title of object
  '() (list ElToY-object)
  "Add a title to an object")


; :title message  displays/sets title
(defmeth titled-object :title (&optional (title nil set))
   (if set (setf (slot-value 'title) title))
   (slot-value 'title))
(send titled-object :documentation :title
      "Returns or sets title of object")


;; :print/:describe

(defmeth titled-object :print (&optional (stream t))
   (format stream "#<Titled-object: ~S>"
	   (send self :title)))

(defmeth titled-object :describe (&optional (stream t) (verbose t))
  (if verbose
      (format stream "Titled-object: title---~S~%"
	      (send self :title)))
  (call-next-method stream verbose))


;; :isnew method
(defmeth titled-object :isnew
         (&rest args &key (title "") &allow-other-keys)
   (setf (slot-value 'title) title)
   (call-next-method args))
(send titled-object :documentation :isnew
      (strcat
       "Titled-object  :title --- sets title
"
       (send ElToY-object :documentation :isnew)))

;;;**********************************************************************
;;; named-object --almost the same, but its a name, not a title
;;;**********************************************************************
(defproto named-object 
  '(name)				;name of object
    '() (list ElToY-object)
  "Add a name to an object")


; :name message  displays/sets name
(defmeth named-object :name (&optional (name nil set))
   (if set (setf (slot-value 'name) name))
   (slot-value 'name))
(send named-object :documentation :name
      "Returns or sets name of object")


;; :print/:describe
(defmeth named-object :print (&optional (stream t))
   (format stream "#<Named-object: ~S>"
	   (send self :name)))


(defmeth named-object :describe (&optional (stream t) (verbose t))
  (if verbose
      (format stream "Named-object: name---~S~%"
	      (send self :name)))
  (call-next-method stream verbose))



;; :isnew method
(defmeth named-object :isnew
         (&rest args &key (name "") &allow-other-keys)
  (setf (slot-value 'name) name)
  (call-next-method args))

(send named-object :documentation :isnew
      (strcat
       "Named-object  :name --- sets name
"
       (send ElToY-object :documentation :isnew)))

  

;;; error messages

; user error (for later signaling user errors (possibly as pop-up
;							boxes))

(defun uerror (continue-message error-format &rest args)
  (let ((error-message (apply #'format nil error-format args)))
    (cerror continue-message error-message )))


;;; constants

(defvar *infty* 377777 "a big number")

(defvar *-infty* -377777 " a big negative number")

(defvar *epsilon* 0.00001 "a small number")

(defvar *-epsilon* -0.00001 " a small negative number")


(new-provide :el-utils)
