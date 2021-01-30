;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: :PROJECT-DOC -*-

;;;  File Name          :  project-doc-proto.lsp
;;;  Version Number     :  1.1
;;;  Last Changed Date  :  10/17/98 
;;;  Created Date       :  10/17/98
;;;
;;; Copyright 1998. Russell G. Almond and Thomas Dye
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

(in-package :df)

;version-doc-proto
(xlos:defclass version-doc-proto ()
	       ((major :accessor :major :initarg :major :type integer
		       :initform 0
		       :documentation "The 1 in version 1.0.2.")
		(minor :accessor :minor :initarg :minor :type integer
		       :initform 0
		       :documentation "The 0 in version 1.0.2.")
		(rev-level :accessor :rev-level :initarg :rev-level :type integer
		       :initform 0
		       :documentation "The 0 in version 1.0.2.")
		(date :accessor :date :initarg :date :type string
		      :initform "Undated"
		      :documentation "The date of the release.")
		(release :accessor :release :initarg :release :type string
			 :initform ""
			 :documentation "a = alpha, b = beta, c = gamma for code 
version; perhaps some
other scheme for xls-version, say: x = code should work with this
xls version ; y = code tested and works with this xls version ; z =
code doesn't work with this xls version ")
)
		(:documentation "This object holds information on
versions.  It is used to hold the version information for the code and
for xls.  The meaning of the release slot changes with the context.")  
		)

(defmeth Version-doc-proto :print (&optional (stream t))
  "A generic method for printing Definition objects."
  (declare  (type (or Stream T Nil) stream)
	    (:returns self))
  (format stream (if *print-escape*
		     "#<Version: ~d.~d.~d~a (~A)>"
		   "Version ~d.~d.~d~a (~A)")
	  (send self :major)
	  (send self :minor)
	  (send self :rev-level)
	  (send self :release)
	  (send self :date))
  self)

(defgeneric version> (ver1 ver2)
  (declare (type Version-Doc-Proto ver1 ver2)
	   (returns (member '(T Nil))))
  (:documentation "Returns T if <ver1> is more recent than <ver2>"))

(defmethod version> ((ver1 Version-doc-proto) ver2)
  (cond
   ((> (send ver1 :major) (send ver2 :major)) t)
   ((< (send ver1 :major) (send ver2 :major)) nil)
   ((> (send ver1 :minor) (send ver2 :minor)) t)
   ((< (send ver1 :minor) (send ver2 :minor)) nil)
   ((> (send ver1 :rev-level) (send ver2 :rev-level)) t)
   ((< (send ver1 :rev-level) (send ver2 :rev-level)) nil)
   (t (> (send ver1 :release) (send ver2 :release)))))

;author-doc-proto
(xlos:defclass author-doc-proto ()
	       ((name :accessor :name :initarg :name :type string 
		      :initform ""
		      :documentation "Author's name.")
		(affiliation :accessor :affiliation 
			     :initarg :affiliation :type string 
			     :initform ""
		      :documentation "Author's Institution.")
		(email :accessor :email :initarg :email :type string 
		      :initform ""
		      :documentation "Author's email address.")
		(home-page :accessor :home-page :initarg :home-page :type string 
		      :initform ""
		      :documentation "Author's home-page URL.")
		)
	       (:documentation "This object holds information about an author or reviewer of the code.")
	       )


;project-doc-proto
(xlos:defclass project-doc-proto ()
	       ((title :accessor :title :initarg :title :type string 
		      :initform ""
		      :documentation "Title of the code.")
		(authors :accessor :authors :initarg :authors
			    :type list
			    :initform ()
			    :documentation "A list of author-doc-proto describing the author(s) of the code.")
		(version :accessor :version :initarg :version
			 :type version-doc-proto
			 :initform (send version-doc-proto :new)
			 :documentation "Version of the code.")
		(package :accessor :package :initarg :package
			 :type (or String T)
			 :initform "USER"
			 :documentation "Name of the package these
functions live in.  Warning:  leaving your module in the user package
could result in name conflicts with other packages and will make it
more difficult for the automatic documentation process to work.")
		(nicknames :accessor :nicknames :initarg :nicknames
			   :type list
			   :initform ()
			   :documentation "A list of package nicknames.")
		(package-dependencies :accessor :package-dependencies :initarg
				      :package-dependencies
				      :type list
				      :initform ()
				      :documentation "List of package dependencies.")
		(motivation :accessor :motivation :initarg :motivation
			    :type string
			    :initform ""
			    :documentation "Explanation why the code was written.")
		(functional-description :accessor :functional-description 
					:initarg :functional-description
					:type string
					:initform ""
					:documentation "What the code is supposed to accomplish.")
		(stat-description :accessor :stat-description 
				  :initarg :stat-description
				  :type string
				  :initform ""
				  :documentation "Description of the statistical idea behind the code")
		(exported-objects :accessor :exported-objects
				  :initarg :exported-objects
				  :type list
				  :initform ()
				  :documentation "A list of objects exported by the package.")
		(exported-functions :accessor :exported-functions
				    :initarg :exported-functions
				    :type list
				    :initform ()
				    :documentation "A list of functions exported by the package.")
		(global-vars :accessor :global-vars :initarg
			     :global-vars :type list
			     :initform ()
			     :documentation "A list of var-doc-proto that describe global variables used by the package.")
		(instructions :accessor :instructions :initarg :instructions
			    :type string
			    :initform ""
			    :documentation "User instructions for the package.")
		(examples :accessor :examples :initarg :examples
			    :type list
			    :initform ()
			    :documentation "A list of strings containing examples of common uses for the package.")
		(xls-version :accessor :xls-version :initarg :xls-version
			     :type string
			     :initform "No known incompatabilities"
			     :documentation "A list of version-doc-proto containing information on versions of xls that are compatible and incompatible with the package.")
		(os-dependencies :accessor :os-dependencies :initarg 
				 :os-dependencies
				 :type string
				 :initform "No known incompatabilities."
				 :documentation "Description of operating system dependencies.")
		(warnings :accessor :warnings :initarg :warnings
			     :type list
			     :initform ()
			     :documentation "A list of strings, each with a caveat for the well-intentioned but possibly naive user.")
		(change-history :accessor :change-history :initarg :change-history
			     :type list
			     :initform ()
			     :documentation "A list of strings, each
describing the changes since the last version.")
		(see-also :accessor :see-also :initarg :see-also
			     :type list
			     :initform ()
			     :documentation "A list of strings, each containing a helpful alternative to the package.")
		(references :accessor :references :initarg :references
			     :type list
			     :initform '()
			     :documentation "A list of strings, each with a reference to a book, article, paper, etc. of likely interest.")
		(copyright :accessor :copyright :initarg :copyright
			    :type string
			    :initform ""
			    :documentation "String describing
copyright information and licence to include in library including any
restrictions on use (i.e., educational and research only, or no
redistribution without permission.")
		(reviewed-by :accessor :reviewed-by :initarg :reviewed-by
			     :type author-doc-proto
			     :initform (send author-doc-proto :new)
			     :documentation "Who reviewed the package for the archive.")
		;; RGA --- Added these in part to make the
		;; autodocumenting features fly.
	    (files :accessor :files :initarg :files
	    	:type list :initform ()
	    	:documentation "A list of strings each with the name of a file 
	    	(relative to documentation file) contained in the project.  
	    	Autodocumentation feature uses these files for definitions.")
	    (source-home :accessor :source-home :initarg :source-home
	    	:type Pathname 
	    	:initform (make-pathname :directory (pathname-directory *load-truename*))
	    	:documentation 
	    	"This should automatically capture the location of a project so we 
	    	can do the appropriate source grabbing for the definitions.  Users 
	    	should not need to set this, it should automatically be set when the
	    	project-doc instance object is created.  This in rather assumes the the 
	    	project-doc is created while reading a file in the top level 
	    	directory of the project.")
)
(:documentation "This is the basic object for documenting a project.  This object
can be used to produce printed documentation (via TeX) or on-line
documentation (via HTML).")
)


;;; Autobuild documentation
;;; We should be able to use the defintions stuff to automatically
;;; create a lot of the definition macros for the project-doc-proto

(defgeneric build-project-definitions (doc)
  (declare (type Project-Doc-Proto doc)
	   (:returns (values doc defs)))	    
  (:documentation
   "This automatically extracts the exported object definitions from
the source files for a project and fills in the appropriate
documenation fields"))


(defmethod build-project-definitions ((doc Project-Doc-Proto))
  (declare (type Project-Doc-Proto doc)
	   (:returns (values doc defs)))	    
   "This automatically extracts the exported object definitions from
the source files for a project and fills in the appropriate
documenation fields"
   (let* ((dir (send doc :source-home))
	  (files 
	   (mapcar #'(lambda (f) 
		       (merge-pathnames f dir))
		   (send doc :files)))
	  (defs (read-definitions-from-files files))
	  (exported (remove-if-not #'exported-definition? defs)))
     (send doc :exported-objects 
	   (sort (remove-if-not #'(lambda (x)
				    (kind-of-p x user-type-definition))
				exported)
		 #'definition-alpha<))
     (send doc :exported-functions 
	   (sort (remove-if-not 
		  #'(lambda (x)
		      ;; Note:  message and method defintions are
		      ;; generally uninteresting.
		      (and (kind-of-p x lambda-list-definition)
			   (not (kind-of-p x method-definition))))
		  exported)
		 #'definition-alpha<)) 
     (send doc :global-vars 
	   (sort (remove-if-not #'(lambda (x)
				    (kind-of-p x global-variable-definition))
				exported)
		 #'definition-alpha<))

     (values doc defs)))


