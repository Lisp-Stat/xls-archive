;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: :Definitions -*-
;;; RGA modifications to JAM's definition objects
;;;  File Name          :  tex-print.lisp
;;;  Version Number     :  1.5
;;;  Last Changed Date  :  95/11/14 At 16:49:34
;;;  Created Date       :  11/14/95

(in-package :Definitions)

(defun to-tex-string (obj)
  "Generates a suitable printed representation of an object with
appropriate escapes."
  (fix-TeX-string
   (format nil "~:(~s~)" obj)))


(defgeneric print-tex-documentation (def path &key mode brief
					 include-user-manual )
  (declare (type Project-Doc-Proto doc)
	   (type (or String Pathname) path)
	   (type (member '(:Tex :Latex)) mode)
	   (type (or T Null) brief include-user-manual)
	   (:returns defs))	    
  (:documentation
   "Print a TeX representation of the project documenation <do>
on the file corresponding to <path>.  <Mode> should be either :tex for
Plain TeX or :latex for LaTeX."))

(defmethod print-tex-documentation ((doc Project-Doc-Proto) path 
					&key (mode :tex)
					(brief nil)
					(include-user-manual nil))

  "Print a TeX representation of the project documenation <do>
on the file corresponding to <path>.  

<Mode> should be either :tex for Plain TeX or :latex for LaTeX.
If <Brief> is non-nil, then only the functional docs will be printed."

  (declare (type Project-Doc-Proto doc)
	   (type (or String Pathname) path)
	   (type (member '(:Tex :Latex)) mode)
	   (type (or T Null) brief include-user-manual)
	   (:returns defs))

  (with-open-file (s path
		   :direction :output
		   :if-does-not-exist :create
		   :if-exists :supersede)
    (format s "\\input ~A~%\\hfuzz=~Dpt~%~%"
	    (if (eql mode :tex)
		user::*definition-tex-header-file*
	      user::*definition-latex-header-file*)
	    *definition-hfuzz*)
    ;; Minimal Docs
    (format s "~%\\beginProject{ ~A}~%" (send doc :title))
    (print-tex-documentation-title doc :stream s)
    (print-tex-documentation-authors doc :stream s)
    (print-tex-documentation-version doc :stream s)
    (print-tex-documentation-motivation doc :stream s)
    (print-tex-documentation-copyright doc :stream s)
    (print-tex-documentation-reviewers doc :stream s)
    ;; Expanded descriptions
    (when (not brief)
      (print-tex-documentation-functional doc :stream s)
      (print-tex-documentation-stat doc :stream s)
      (print-tex-documentation-refs doc :stream s)
      (print-tex-documentation-instructions doc :stream s)
      (print-tex-documentation-examples doc :stream s)
      (print-tex-documentation-warnings doc :stream s)
      (print-tex-documentation-see doc :stream s)
      (print-tex-documentation-xls doc :stream s)
      (print-tex-documentation-os doc :stream s)
      (print-tex-documentation-dependencies doc :stream s)
      ;; Programmer/user level docs
      (print-tex-documentation-package doc :stream s) ;; package and nicknames
      (print-tex-documentation-changes doc :stream s)
      (print-tex-documentation-files doc :stream s)
      (print-tex-documentation-objects doc :stream s
				       :complete include-user-manual)
      (print-tex-documentation-funs doc :stream s
				    :complete include-user-manual)
      (print-tex-documentation-vars doc :stream s
				    :complete include-user-manual)
      )
    (when include-user-manual
      (print-tex-documentation-object-docs doc :stream s)
      (print-tex-documentation-fun-docs doc :stream s)
      (print-tex-documentation-var-docs doc :stream s))
    (format s "\\endProject~%~%")
    )
  (values doc))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic Printing features



(defgeneric print-tex-documentation-title (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the title of this project description.")
  )

(defmethod print-tex-documentation-title ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (tex format) the title of this project description."
  (format stream "\\projectTitle{ ~A}~%" (send doc :title))
  doc)




(defgeneric print-tex-documentation-authors (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the authors of this project description.")
  )

(defmethod print-tex-documentation-authors ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (tex format) the authors of this project description."
  (dolist (author (send doc :authors))
    (format stream "\\projectAuthor{ ~A}{ ~A}{ ~A}~%" 
	    (send author :name) (send author :affiliation)
	    (send author :email)))
  doc)

(defgeneric print-tex-documentation-version (doc &key stream)
  (declare (type (or Project-Doc-Proto Version-Doc-Proto) doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the version information for
this project description.") 
  )

(defmethod print-tex-documentation-version ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (tex format) the version of this project description."
  (format stream "\\projectVersion{ ~A}~%" (send doc :version))
  doc)


(defgeneric print-tex-documentation-functional (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the functional of this project description.")
  )

(defmethod print-tex-documentation-functional ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (tex format) the functional of this project description."
  (format stream "\\beginFunctionalDesc~% ~A ~%\\endFunctionalDesc~%" 
	  (send doc :functional-description))
  doc)

(defgeneric print-tex-documentation-copyright (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the copyright of this project description.")
  )

(defmethod print-tex-documentation-copyright ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (tex format) the copyright of this project description."
  (format stream "\\projectCopyright{ ~A}~%" (send doc :copyright))
  doc)

(defgeneric print-tex-documentation-reviewers (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the reviewers of this project description.")
  )

(defmethod print-tex-documentation-reviewers ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (tex format) the reviewers of this project description."
  (let ((reviewer (send doc :reviewed-by)))
    (format stream "\\projectReviewer{ ~A}{ ~A}~%" 
	    (send reviewer :name) 
	    (send reviewer :email)))
  doc)


;; Expanded descriptions

(defgeneric print-tex-documentation-motivation (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the motivation of this project description.")
  )

(defmethod print-tex-documentation-motivation ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (tex format) the motivation of this project description."
  (format stream "\\beginMotivationDesc~% ~A ~%\\endMotivationDesc~%" 
	  (send doc :motivation))
  doc)

(defgeneric print-tex-documentation-stat (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the stat of this project description.")
  )

(defmethod print-tex-documentation-stat ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (tex format) the stat of this project description."
  (format stream "\\beginStatDesc~% ~A ~%\\endStatDesc~%" 
	  (send doc :stat-description))
  doc)

(defgeneric print-tex-documentation-refs (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the refs of this project description.")
  )

(defmethod print-tex-documentation-refs ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (tex format) the refs of this project description."
  (format stream "\\beginRefs~%")
  (dolist (ref (send doc :references))
    (format stream "\\projectRef{ ~A}~%" ref))
  (format stream "\\endRefs~%")
  doc)

(defgeneric print-tex-documentation-instructions (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the instructions of this project description.")
  )

(defmethod print-tex-documentation-instructions ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (tex format) the instructions of this project description."
  (format stream "\\beginInstructions~% ~A ~%\\endInstructions~%" 
	  (send doc :instructions))
  doc)

(defgeneric print-tex-documentation-examples (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the examples of this project description.")
  )

(defmethod print-tex-documentation-examples ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (tex format) the examples of this project description."
  (format stream "\\beginExamples~%")
  (dolist (ex (send doc :examples))
    (format stream "\\projectEx{ ~A}~%" ex))
  (format stream "\\endExamples~%")
  doc)

(defgeneric print-tex-documentation-warnings (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the warnings of this project description.")
  )

(defmethod print-tex-documentation-warnings ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (tex format) the warnings of this project description."
  (format stream "\\beginWarnings~%")
  (dolist (war (send doc :warnings))
    (format stream "\\projectWarn{ ~A}~%" war))
  (format stream "\\endWarnings~%")
  doc)

(defgeneric print-tex-documentation-changes (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the change history of this project.")
  )

(defmethod print-tex-documentation-changes ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (tex format) the change history of this project description."
  (format stream "\\beginChanges~%")
  (dolist (change (send doc :change-history))
    (format stream "\\projectChange{ ~A}~%" change))
  (format stream "\\endChanges~%")
  doc)

(defgeneric print-tex-documentation-see (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the see of this project description.")
  )

(defmethod print-tex-documentation-see ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (tex format) the see of this project description."
  (format stream "\\beginSeeAlsos~%")
  (dolist (see (send doc :see-also))
    (format stream "\\projectSee{ ~A}~%" see))
  (format stream "\\endSeeAlsos~%")
  doc)

(defgeneric print-tex-documentation-xls (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the xls of this project description.")
  )

(defmethod print-tex-documentation-xls ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (tex format) the xls of this project description."
  (format stream "\\projectXLSVersion{ ~A}~%" (send doc :xls-version))
  doc)

(defgeneric print-tex-documentation-os (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the os of this project description.")
  )

(defmethod print-tex-documentation-os ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (tex format) the os of this project description."
  (format stream "\\projectOSVersion{ ~A}~%" (send doc :os-dependencies))
  doc)

(defgeneric print-tex-documentation-dependencies (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the dependencies of this project description.")
  )

(defmethod print-tex-documentation-dependencies ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (tex format) the dependencies of this project description."
  (let ((deps (send doc :package-dependencies)))
    (when deps
      (format stream "\\beginDepList \\cd{ ~{~A \\hfudge ~}} \\endProjectList~%"
	      (mapcar #'to-TeX-string deps))))
  doc)

      ;; Programmer/user level docs
(defgeneric print-tex-documentation-package (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the package of this project description.")
  )

(defmethod print-tex-documentation-package ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (tex format) the package of this project description."
  (format stream "\\projectPackage{ ~A}{~{~A \\hfudge ~}}~%" 
	  (to-tex-string (send doc :package))
	  (mapcar #'to-tex-string (send doc :nicknames)))
  doc)

(defgeneric print-tex-documentation-files (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the files of this project description.")
  )

(defmethod print-tex-documentation-files ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  "Prints (tex format) the files of this project description."
  (let ((files (send doc :files)))
   (when files
     (format stream "\\beginFileList \\cd{~{~A \\hfudge ~}} \\endProjectList~%"
	     (mapcar #'to-tex-string files)))
   doc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric print-tex-documentation-objects (doc &key stream
						 complete)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the list of objects of in this
project.")
  )

(defmethod print-tex-documentation-objects ((doc Project-Doc-Proto) 
					    &key (stream *standard-output*)
					    (complete nil))
  "Prints (tex format) the objects of this project description."
  (let ((objects (send doc :exported-objects)))
    (when objects
      (format stream "\\beginObjectList \\cd{~{~A \\hfudge ~}} \\endProjectList~%"
	      (mapcar #'(lambda (obj)
			  (to-tex-string (definition-name obj)))
		      objects)))
    doc))


(defgeneric print-tex-documentation-object-docs (doc &key stream
						 )
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the documentation for each
objects in the project.")
  )

(defmethod print-tex-documentation-object-docs ((doc Project-Doc-Proto) 
					    &key (stream *standard-output*)
					    )
  "Prints (tex format) the objects of this project description."
  (let ((objects (send doc :exported-objects)))
   (when objects
     (format stream "\\beginObjects~%")
     (dolist (obj objects)
       (print-tex-definition obj :stream stream)))
   doc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric print-tex-documentation-funs (doc &key stream complete)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the funs of this project description.")
  )

(defmethod print-tex-documentation-funs ((doc Project-Doc-Proto) 
					 &key (stream *standard-output*)
					 (complete nil))
  "Prints (tex format) the funs of this project description."
  (let ((objects (send doc :exported-functions)))
   (when objects
     (format stream "\\beginFunList \\cd{~{~A \\hfudge ~}} \\endProjectList~%"
	     (mapcar #'(lambda (obj)
			 (to-tex-string (definition-name obj)))
		     objects)))
   doc))

(defgeneric print-tex-documentation-fun-docs (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the funs of this project description.")
  )

(defmethod print-tex-documentation-fun-docs ((doc Project-Doc-Proto) 
					 &key (stream *standard-output*))
  "Prints (tex format) the funs of this project description."
  (let ((objects (send doc :exported-functions)))
   (when objects
     (format stream "\\beginProjectFuns~%")
     (dolist (obj objects)
       (print-tex-definition obj :stream stream)))
   doc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric print-tex-documentation-vars (doc &key stream complete)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the vars of this project description.")
  )

(defmethod print-tex-documentation-vars ((doc Project-Doc-Proto) 
					 &key (stream *standard-output*)
					 (complete nil))
  "Prints (tex format) the vars of this project description."
  (let ((objects (send doc :global-vars)))
   (when objects
     (format stream "\\beginVarList \\cd{~{~A \\hfudge ~}} \\endProjectList~%"
	     (mapcar #'(lambda (obj)
			 (to-tex-string (definition-name obj)))
		     objects)))
   doc))

    
(defgeneric print-tex-documentation-var-docs (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation "Prints (tex format) the vars of this project description.")
  )

(defmethod print-tex-documentation-var-docs ((doc Project-Doc-Proto) 
					 &key (stream *standard-output*)
					 )
  "Prints (tex format) the vars of this project description."
  (let ((objects (send doc :global-vars)))
   (when objects
     (format stream "\\beginProjectVars~%")
     (dolist (obj objects)
       (print-tex-definition obj :stream stream)))
   doc))

    

