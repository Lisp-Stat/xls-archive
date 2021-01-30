;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: :Definitions -*-
;;;  File Name          :  read.lisp
;;;  Version Number     :  1.2
;;;  Last Changed Date  :  93/08/20 At 19:54:18
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
;;; reading
;;;=======================================================

(defun read-definitions-from-file (path)

  "Read all the forms in the file corresponding to <path>, and create
and return a list of definition objects corresponding to the result of
evaluating those forms (for which <definer-class> does not return
<nil>)."

  (declare (type (or String Pathname) path)
	   (:returns (type List)))

  (setf path (merge-pathnames (merge-pathnames (pathname path))
			      (pathname "foo.lsp")))
  (let ((definitions ())
	(eof (gensym))
	(form ())
	(old-package *package*))
    (unwind-protect
	(with-open-file (s path :direction :input)
	  (loop
	    (setf form (read s nil eof nil))
	    (setf definitions
	      (nconc (cond ((eq form eof) (return))
			   ((atom form) nil)
			   ((eq (first form) 'in-package)
			    (setf *package* (find-package (second form)))
			    nil)
			   (t (make-definitions form path)))
		     definitions))))
      (setf *package* old-package))
    definitions))

(defun read-definitions-from-files (paths)

  "Call <read-definitions-from-file> on each path in <paths>,
concatenating together the results."

  (declare (type List path)
	   (:returns (type List)))

  (mapcan #'read-definitions-from-file paths))

