;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: :User -*-

;;;  File Name          :  project-doc-proto.lsp
;;;  Version Number     :  1.1
;;;  Last Changed Date  :  10/17/98 
;;;  Created Date       :  10/17/98
;;;
;;; Copyright 1998. Russell G. Almond
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

;;; This file is template of the project documentation features.  Fill
;;; out this form to complete the documentation for your project.
;;; Below is a script for running it.

;;; For an example, see the file doc-doc.lsp

(in-package :xlos)

(defparameter *xlos-doc*
  (send df:project-doc-proto :new
	:title "XLOS:  CLOS Macros for XLISP objects"
	:authors 
	(list (send df:author-doc-proto :new :name "Russell Almond" 
		    :affiliation "Educational Testing Service"
		    :email "almond@acm.org" 
		    :home-page
  "http://bayes.stat.washington.edu/almond/almond.html") 
	      )
	:version 
	(send df:version-doc-proto :new :major 1 :minor 0  :rev-level 1
	      :date "Dec 18, 1998")
	:package "XLOS"
	:nicknames (list )
	:package-dependencies (list "LISP")
	:motivation "(1) To provide a series of tools for easily
  translating code written in CLOS (Common Lisp Object System) to the
  XLISP object system.  (2) To provide some of the features of the
  Defclass macro, such as automatic creation of accessor functions and
  initialization functions and tightly coupled object defintition and
  documentation."
	:functional-description "Provides a Defclass macro for
  defining prototypes and simplified versions of the defgeneric and
  defmethod macros.  (These are limited to one dispatching argument
  which must be a prototype.)  The macros work by translating the CLOS
  style definitions into corresponding defproto, defun and defmeth
  macros.  Thus XLOS only provides the syntatic sugar of CLOS while
  retaining the prototype-instance flavor of XLISP."
	:stat-description "Providing a self-documenting defclass form
  (as opposed to the more limited defprotot) will encourage better
  documentation of XLS objects and will work well with the Definitions
  documentation package.  It also simpilifies translation of Common
  Lisp packages to XLS."
	:instructions "Including XLOS in the package use list for your
  package gives you access to the defclass, defgeneric and defmethod
  macros.  Syntax for these is described in Steele[1990] (or other
  Common Lisp references."
	:examples (list "(xlos:defclass author-doc-proto ()
	       ((name :accessor :name :initarg :name :type string 
		      :initform \"\"
		      :documentation \"Author's name.\")
		(affiliation :accessor :affiliation 
			     :initarg :affiliation :type string 
			     :initform \"\"
		      :documentation \"Author's Institution.\")
		(email :accessor :email :initarg :email :type string 
		      :initform \"\"
		      :documentation \"Author's email address.\")
		(home-page :accessor :home-page :initarg :home-page :type string 
		      :initform \"\"
		      :documentation \"Author's home-page URL.\")
		)
	       (:documentation \"This object holds information about an author or reviewer of the code.\")
	       )"
	       "The forgoing example would automatically create a
  defproto statement, and defmeths for the :isnew, :name, etc
  methods."
	       "(defgeneric print-tex-documentation-title (doc &key stream)
  (declare (type Project-Doc-Proto doc)
	   (type Stream  stream)
	   (:returns (type Project-Doc-Proto doc)))
  (:documentation \"Prints (tex format) the title of this project description.\")
  )
"
  "The second example defines an alias function for (send doc
  print-tex-documenation :stream stream)"
  "(defmethod print-tex-documentation-title ((doc Project-Doc-Proto) 
					&key (stream *standard-output*))
  \"Prints (tex format) the title of this project description.\"
  (format stream \"\\projectTitle{ ~A}~%\" (send doc :title))
  doc)"
  "The last example is equivalent to (defmeth Project-Doc-Proto
  print-tex-documenation-title ...)"
  )
	:xls-version "Tested with 3.52.5; unlikely to work with
  earlier versions."
	:os-dependencies "No known incompatabilities; tested on Unix
  (Sun Solaris) and Mac."
	:warnings (list "Only syntatic sugar, still uses XLS object
  system."
			"Only a subset of CLOS.  In particular, does
  not allow for dispatch on multiple objects, or non-object types.
  XLS type system and object system are not integerated."
			"Only allows generic functions to dispatch on
  one argument."
			"Adds slot XLOS:SLOT-TYPES and method
  :slot-type to *object*.  This may conflict with other packages.")
	:see-also (list "Definitions package contains
  examples of usage."
			"Kiczales, des Riveres and Bobrow [1991]
  contains an alternative CLOSette implementation.")
	:references (list "Steele, G.L., Jr. [1990] ``Common LISP:  The
Language, Second Edition.'' Digital Press."
			  "Kiczales, G., J. des Rivieres, and D.G. Bobrow [1991]``The Art of the Metaobject Protocol.''  MIT Press. ")
	:copyright "This work is public domain.  The authors make no
warantee as to its suitability for any particular purpose."
	:change-history
	(list "Version 1.0.1 Added shared-initialize generic function.")
	:files (list 
		(merge-pathnames "defclass.lsp" user::*xlos-directory*)
		(merge-pathnames "defgeneric.lsp" user::*xlos-directory*))
	)
  "This is the Documenation Object for the XLOS object system interface.")

(df:build-project-definitions *xlos-doc*)


;;; Printing Script
;;; Uncomment those lines you want to execute
;;; Note:  This script assumes that the definition package has
;;; previously been loaded.

;; Print project summary, TeX mode:
(df:print-tex-documentation *xlos-doc* 
    (merge-pathnames "brief.tex" user::*xlos-directory*)
    :mode :tex :brief t)
;; Print project description, TeX model:
(df:print-tex-documentation *xlos-doc* 
    (merge-pathnames "doc.tex" user::*xlos-directory*)
    :mode :tex :brief nil)
;; Print user manual, TeX model:
(df:print-tex-documentation *xlos-doc* 
    (merge-pathnames "user.tex" user::*xlos-directory*)
    :mode :tex :brief nil :include-user-manual t)

;; Print project summary, html mode:
(df:print-html-documentation *xlos-doc* 
     (merge-pathnames "brief.html" user::*xlos-directory*)
     :brief t)
;; Print project description, html model:
(df:print-html-documentation *xlos-doc* 
     (merge-pathnames "doc.html" user::*xlos-directory*)
     :brief nil)
;; Print user manual, html model:
(df:print-html-documentation *xlos-doc* 
     (merge-pathnames "user.html" user::*xlos-directory*)
     :brief nil :include-user-manual t)


;;; Manual building scripts.

(defparameter *xlos-defs*
       (df:read-definitions-from-files (send *xlos-doc* :files))
  "All definitions")

(df:print-tex-definitions
 (sort (remove-if-not #'df:exported-definition? *xlos-defs*)
       #'df:definition-alpha<)
 (merge-pathnames "reference-manual.tex" user::*xlos-directory*) :mode :tex)


(df:print-html-definitions
 (sort (remove-if-not #'df:exported-definition? *xlos-defs*)
       #'df:definition-alpha<)
 (merge-pathnames "reference-manual.html" user::*xlos-directory*))







	