;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: :Definitions -*-
;;; RGA modifications to JAM's definition objects
;;;  File Name          :  html-print-doc.lisp
;;;  Version Number     :  1.0
;;;  Last Changed Date  :  98/12/01
;;;  Created Date       :  98/12/01

(in-package :Definitions)

(defgeneric print-html-documentation (def path &key brief
					  include-user-manual)
  (declare (type Project-Doc-Proto doc)
	   (type (or String Pathname) path)
	   (type (or T Null) brief include-user-manual)
	   (:returns defs))	    
  (:documentation
   "Print an html representation of the project documentation <do>
on the file corresponding to <path>."))

#|
ael: Here's the point at which I started. I copied Russell's functions from
tex-print-doc.lsp, and I'll now alter them as needed.
|#

(defun to-html-string (obj)
  "Generates a suitable printed representation of an object with
appropriate escapes."
  (fix-Html-string
   (format nil "~:(~s~)" obj)))

(defun html-tag-name (def)
  "Generates a name for the definition tagged with the appropriate
href tag and anchor."
  (format nil "<a href=\"#~A\">~A</a>"
	  (definition-name->tag def)
	  (fix-html-string (definition-name->string def))))


(defmethod print-html-documentation ((doc Project-Doc-Proto) path 
					&key
					(brief nil)
					(include-user-manual nil))

  "Print a Html representation of the project documenation <do>
on the file corresponding to <path>.  

If <Brief> is non-nil, then only the functional docs will be printed."

  (declare (type Project-Doc-Proto doc)
	   (type (or String Pathname) path)
	   (type (or T Null) brief include-user-manual)
	   (:returns defs))

  (with-open-file
   (s path
      :direction :output
      :if-does-not-exist :create
      :if-exists :supersede)
   
   (format s "
<html>
<head>
<title>~a</title>
</head>
"(send doc :title))
	    
   ;; Minimal Docs
   (print-html-documentation-title doc :stream s)
   (print-html-documentation-authors doc :stream s)
   (print-html-documentation-version doc :stream s)
   (print-html-documentation-motivation doc :stream s)
   (print-html-documentation-copyright doc :stream s)
   (print-html-documentation-reviewers doc :stream s)
   ;; Expanded descriptions
   (when (not brief)
     (print-html-documentation-functional doc :stream s)
     (print-html-documentation-stat doc :stream s)
     (print-html-documentation-refs doc :stream s)
     (print-html-documentation-instructions doc :stream s)
     (print-html-documentation-examples doc :stream s)
     (print-html-documentation-warnings doc :stream s)
     (print-html-documentation-see doc :stream s)
     (print-html-documentation-xls doc :stream s)
     (print-html-documentation-os doc :stream s)
     (print-html-documentation-dependencies doc :stream s)
     ;; Programmer/user level docs
     (print-html-documentation-package doc :stream s) ;; package and nicknames
     (print-html-documentation-changes doc :stream s)
     (print-html-documentation-files doc :stream s)
     (print-html-documentation-objects doc :stream s
				       :complete include-user-manual)
     (print-html-documentation-funs doc :stream s
				    :complete include-user-manual)
     (print-html-documentation-vars doc :stream s
				    :complete include-user-manual)
	 )
   (when include-user-manual
     (print-html-documentation-object-docs doc :stream s)
     (print-html-documentation-fun-docs doc :stream s)
     (print-html-documentation-var-docs doc :stream s))
   )
  (values doc))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic Printing features

(defgeneric print-html-documentation-title (doc &key stream)
  (declare
   (type Project-Doc-Proto doc)
   (type Stream  stream)
   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the title of this project description.")
  )

(defmethod print-html-documentation-title 
  ((doc Project-Doc-Proto) &key (stream *standard-output*))
  "Prints (html format) the title of this project description."
  (format stream "<center><h1>~A</h1></center>~%" (send doc :title))
  doc)


(defgeneric print-html-documentation-authors (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the authors of this project description.")
  )

(defmethod print-html-documentation-authors ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (html format) the authors of this project description."
  (dolist (author (send doc :authors))
    (if (> (length (send author :home-page)) 0)
	(format stream 
		"<center><h3><it><a href=\"~a\">~A,</a> ~a</it> <code><a href=\"mailto:~a\">~a</a></code></h3></center>~%"
		(send author :home-page)
		(send author :name)
		(send author :affiliation)
		(send author :email)
		(send author :email))
      (format stream 
		"<center><h3><it>~A, ~a</it> <code><a href=\"mailto:~a\">~a</a></code></h3></center>~%"
		(send author :name)
		(send author :affiliation)
		(send author :email)
		(send author :email))))
  doc)

(defgeneric print-html-documentation-version (doc &key stream)
  (declare (type (or Project-Doc-Proto Version-Doc-Proto) doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the version information for
this project description.") 
  )

(defmethod print-html-documentation-version ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (html format) the version of this project description."
  (format stream "<center><h4>~A</h4></center>~%" (send doc :version))
  doc)


(defgeneric print-html-documentation-functional (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the functional of this project description.")
  )

(defmethod print-html-documentation-functional ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (html format) the functional of this project description."
  (format stream "<p><b>Functional Description:</b>~%~A<p>~%" 
	  (send doc :functional-description))
  doc)

(defgeneric print-html-documentation-copyright (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the copyright of this project description.")
  )

(defmethod print-html-documentation-copyright ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (html format) the copyright of this project description."
  (format stream
	  "<p><b>Copyright and Licence:</b>~%~A<p>~%" 
	  (send doc :copyright))
  doc)

(defgeneric print-html-documentation-reviewers (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the reviewers of this project description.")
  )

(defmethod print-html-documentation-reviewers ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (html format) the reviewers of this project description."
  (let ((reviewer (send doc :reviewed-by)))
    (format stream 
	    "<p><b>Reviewed by:</b>~%~A (<code>~a</code>)<p>~%" 
	    (send reviewer :name) 
	    (send reviewer :email)))
  doc)

;; Expanded descriptions

(defgeneric print-html-documentation-motivation (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the motivation of this project description.")
  )

(defmethod print-html-documentation-motivation ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (html format) the motivation of this project description."
  (format stream
	  "<p><b>Motivation:</b>~%~A<p>~%" 
	  (send doc :motivation))
  doc)

(defgeneric print-html-documentation-stat (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the stat of this project description.")
  )

(defmethod print-html-documentation-stat ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (html format) the stat of this project description."
  (format stream
	  "<p><b>Statistical Description:</b>~%~A<p>~%" 
	  (send doc :stat-description))
  doc)

(defgeneric print-html-documentation-refs (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the refs of this project description.")
  )

(defmethod print-html-documentation-refs ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (html format) the refs of this project description."
  (let ((refs  (send doc :references)))
    (when refs
      (format stream "<p><b>References:</b><p><ol>~%")
      (dolist (ref refs)
	(format stream "<li>~A</li>~%" ref))
      (format stream "</ol><p>~%")
      )
    doc))

(defgeneric print-html-documentation-instructions (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the instructions of this project description.")
  )

(defmethod print-html-documentation-instructions ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (html format) the instructions of this project description."
  (format stream
	  "<p><b>Instructions:</b>~%~A<p>~%" 
	  (send doc :instructions))
  doc)

(defgeneric print-html-documentation-examples (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the examples of this project description.")
  )

(defmethod print-html-documentation-examples
  ((doc Project-Doc-Proto) &key (stream *standard-output*))
  "Prints (html format) the examples of this project description."
  (let ((exs (send doc :examples)))
    (when exs
      (format stream "<p><b>Examples:</b><p><ol>~%")
      (dolist (ex exs)
	(format stream "<li>~A</li>~%" ex))
      (format stream "</ol><p>~%")
      )
    doc))

(defgeneric print-html-documentation-warnings (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the warnings of this project description.")
  )

(defmethod print-html-documentation-warnings ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (html format) the warnings of this project description."
  (let ((wars (send doc :warnings)))
    (when wars
      (format stream "<p><b>Warnings:</b><p><bl>~%")
      (dolist (war wars)
	(format stream "<li>~A</li>~%" war))
      (format stream "</bl><p>~%")
      )
    doc))

(defgeneric print-html-documentation-changes (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the change history of this project.")
  )

(defmethod print-html-documentation-changes ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (html format) the change history of this project description."
  (let ((changes (send doc :change-history)))
    (when changes
      (format stream "<p><b>Change History:</b><p><ol>~%")
      (dolist (change changes)
	(format stream "<li>~A</li>~%" change))
      (format stream "</ol><p>~%")
      )
    doc))



(defgeneric print-html-documentation-see (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the see of this project description.")
  )

(defmethod print-html-documentation-see ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (html format) the see of this project description."
  (let ((sees (send doc :see-also)))
    (when sees
      (format stream "<p><b>See Also:</b><p><ol>~%")
      (dolist (see sees)
	(format stream "<li>~A</li>~%" see))
      (format stream "</ol><p>~%")
      )
    doc))

(defgeneric print-html-documentation-xls (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the xls of this project description.")
  )

(defmethod print-html-documentation-xls ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (html format) the xls of this project description."
  (format stream
	  "<p><b>XLISP-STAT Compatibility Issues:</b><p> ~A<p>~%"
	  (send doc :xls-version))
  doc)

(defgeneric print-html-documentation-os (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the os of this project description.")
  )

(defmethod print-html-documentation-os ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (html format) the os of this project description."
  (format stream
	  "<p><b>Platform Compatibility Issues:</b><p> ~A<p>~%"
	  (send doc :os-dependencies))
  doc)

(defgeneric print-html-documentation-dependencies (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the dependencies of this project description.")
  )

(defmethod print-html-documentation-dependencies ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (html format) the dependencies of this project description."
  (let ((deps (send doc :package-dependencies)))
    (when deps
      (format stream
	      "<p><b>Project depends on the following
additional packages:</b><p>~%~{~A ~}<p>~%" 
	      (mapcar #'to-html-string deps))))
  doc)

      ;; Programmer/user level docs
(defgeneric print-html-documentation-package (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the package of this project
description.") 
  )

(defmethod print-html-documentation-package ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (html format) the package of this project description."
  (format stream 
	  "<p><b>Package:</b>~% ~a ~a<p>~%"
	  (to-html-string (send doc :package))
	  (mapcar #'to-html-string (send doc :nicknames)))
  doc)

(defgeneric print-html-documentation-files (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the files of this project
description.") 
  )

(defmethod print-html-documentation-files ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (html format) the files of this project description."
  (let ((files (send doc :files)))
   (when files
	 (format stream
		 "<p><b>Project source files:</b>~%~A~{, ~A~}<p>~%"
		 ;; This ensures the proper number of commas between entries.
		 (to-html-string (car files))
		 (mapcar #'to-html-string (cdr files))))
   doc))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric print-html-documentation-objects (doc &key stream complete)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the objects of this project description.")
  )

(defmethod print-html-documentation-objects 
  ((doc Project-Doc-Proto) &key (stream *standard-output*) (complete nil))
  "Prints (html format) the objects of this project description."
  (let ((objects (send doc :exported-objects)))
    (when
     objects
     (if complete
	 ;; Need to print with tags
	 (let ((tagged-names (mapcar #'html-tag-name objects)))
	   (format stream
		   "<p><b>Exported Objects:</b><p> ~A~{, ~A~}<p>~%"
		   ;; This ensures the proper number of commas between entries.
		   (car tagged-names) (cdr tagged-names))) 
       (let ((names (mapcar
		     #'(lambda (obj)
			 (to-html-string (definition-name obj)))
		     objects)))
	 (format stream
		 "<p><b>Exported Objects:</b><p> ~A~{, ~A~}<p>~%"
		 ;; This ensures the proper number of commas between entries.
		 (car names) (cdr names))
	 )
       ))
    doc))

(defgeneric print-html-documentation-object-docs (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the objects of this project description.")
  )

(defmethod print-html-documentation-object-docs 
  ((doc Project-Doc-Proto) &key (stream *standard-output*) )
  "Prints (html format) the objects of this project description."
  (let ((objects (send doc :exported-objects)))
    (when
     objects
     (format stream
	     "<hr><center><b><h2>Exported Classes and Prototype Objects:</h2></b></center>~%")
     (dolist (obj objects)
       (print-html-definition obj :stream stream)))
    doc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric print-html-documentation-funs (doc &key stream complete)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the funs of this project description.")
  )

(defmethod print-html-documentation-funs ((doc Project-Doc-Proto) 
					 &key (stream *standard-output*)
					 (complete nil))
  "Prints (html format) the funs of this project description."
  (let ((objects (send doc :exported-functions)))
   (when objects
     (if complete
	 ;; Need to print with tags
	 (let ((tagged-names (mapcar #'html-tag-name objects)))
	   (format stream
		   "<p><b>Exported Functions and Messages:</b><p> ~A~{, ~A~}<p>~%"
		   ;; This ensures the proper number of commas between entries.
		   (car tagged-names) (cdr tagged-names)))
       (let ((names (mapcar #'(lambda (obj)
			   (to-html-string (definition-name obj)))
		       objects)))
	 (format stream
		 "<p><b>Exported Functions and Messages:</b><p> ~A~{, ~A~}<p>~%"
		 ;; This ensures the proper number of commas between entries.
		 (car names) (cdr names)
		 ))
       ))
   doc))

(defgeneric print-html-documentation-fun-docs (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the funs of this project description.")
  )

(defmethod print-html-documentation-fun-docs ((doc Project-Doc-Proto) 
					 &key (stream *standard-output*)
					 (complete nil))
  "Prints (html format) the funs of this project description."
  (let ((objects (send doc :exported-functions)))
   (when objects
     (format stream
	     "<hr><center><b><h2>Exported Functions, Generic Functions and Messages:</h2></b></center>~%")
     (dolist (obj objects)
       (print-html-definition obj :stream stream)))
   doc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric print-html-documentation-vars (doc &key stream complete)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the vars of this project description.")
  )

(defmethod print-html-documentation-vars ((doc Project-Doc-Proto) 
					 &key (stream *standard-output*)
					 (complete nil))
  "Prints (html format) the vars of this project description."
  (let ((objects (send doc :global-vars)))
   (when objects
     (if complete
	 ;; Need to print with tags
	 (let ((tagged-names (mapcar #'html-tag-name objects)))
	   (format stream
		   "<p><b>Global Variables and Parameters:</b><p> ~A~{, ~A~}<p>~%"
		   ;; This ensures the proper number of commas between entries.
		   (car tagged-names) (cdr tagged-names)))
       (let ((names (mapcar
		     #'(lambda (obj) (to-html-string (definition-name obj)))
		     objects)))
	 (format
	  stream
	  "<p><b>Global Variables and Parameters:</b><p>~A~{, ~A~}<p>~%"
	  ;; This ensures the proper number of commas between
	  ;; entries.
	  (car names) (cdr names)))
       ))
   doc))

(defgeneric print-html-documentation-var-docs (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (html format) the vars of this project description.")
  )

(defmethod print-html-documentation-var-docs ((doc Project-Doc-Proto) 
					 &key (stream *standard-output*)
					 )
  "Prints (html format) the vars of this project description."
  (let ((objects (send doc :global-vars)))
    (when objects
      (format stream
	      "<hr><center><b><h2>Global Variables, Parameters and Constants:</h2></b></center>~%")
      (dolist (obj objects)
	(print-html-definition obj :stream stream)))
    doc))

