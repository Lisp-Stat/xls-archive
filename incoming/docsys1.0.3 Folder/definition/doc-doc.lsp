;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: :User -*-

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

;;; This file is an example of the documentation format as well as the
;;; documentation for the definitions package.

(defparameter *doc*
  (send df:project-doc-proto :new
	:title "Project Documentation Toolkit"
	:authors 
	(list (send df:author-doc-proto :new :name "Russell Almond" 
		    :affiliation "Educational Testing Service"
		    :email "almond@acm.org" 
		    :home-page "http://bayes.stat.washington.edu/almond/almond.html")
	      (send df:author-doc-proto :new :name "Thomas S. Dye" 
		    :affiliation "International Archaeological Research Institute, Inc."
		    :email "tdye@lava.net" 
		    :home-page "http://www.lava.net/~tdye")
	      (send df:author-doc-proto :new :name 
		    "Andrew E. Long" :email "aelon@sph.umich.edu")
	      )
	:version 
	(send df:version-doc-proto :new :major 1 :minor 0  :rev-level 3
	      :date "Jan 11, 1999")
	:package "DEFINITIONS"
	:nicknames (list "DF")
	:package-dependencies (list "XLOS" "LISP")
	:motivation "This package is intended to solve two different
  problems.  The first is to give us a database of XLS objects which
  describe project written in XLS.  The second is to provide an
  automatic way of documenting projects."
	:functional-description "This package provides objects for
  documenting projects, as well as objects for documenating functions,
  generic functions, messages, variables, parameters, prototypes,
  etc. which are part of a project.  There are a number of tools
  provided for extracting the documentation automatically from the
  source files (although project level documentation must still be
  created manually.)  Two functions provide a printing mechanism into
  either Plain TeX or HTML."
	:stat-description "Documenting software is critically
  important to the dissemination of statistical ideas, but is an often
  overlooked step in statistical programming.  This package attempts
  to provide tools for automatically documenting projects in a hope to
  eliminate some of that lag.   This is based on an earlier
  implementation of a similar system in Common LISP by John McDonald
  (McDonald[1991])" 
	:instructions "Docuementing a project under the
  definitions system consists of four steps:  (1) writing your code
  according to certain conventions which make it self documenting;
  this includes using declarations where appropriate and including
  documentation strings.  (2) Creating a documentation object for your
  project; this can be done by filling out the template given in the
  file doc.lsp.  (3) load the file 'definitions/system.lsp' and then 
  the file containing your project documentation object. (4)  Run the
  function df:print-tex-documentation or df:print-html-documentation
  to generate the documentation.  TeX documenation comes out in Plain
  TeX or LaTeX." 
	:examples (list "See the file doc-doc.lsp in this distribution.")
	:xls-version "Works with XLS 3.52.5 and later."
	:os-dependencies "No known incompatabilities; tested on Unix
  and Mac"
	:warnings (list "Unless you document your code, it will be
  unreadable and these tools will be useless!"
			"These tools will not work very well if all of
  your code is left in the USER package; in particular, it relies on
  which symbols are exported to know which objects are important."
			"TeX documents will still create overfull
hboxes, especially if doc-strings contain lots of long symbol names.
You can increase hfuzz to fix small problem.  Editing the TeX file is
probably you best resort for big problems."
			)
	:see-also (list "Note Tierney [1990] is not really a complete
  LISP reference manual.  For notes on documentation strings and
  declaration syntax, look at Steele [1990]")
	:references (list "McDonald, J.A. [1991] ``Definitions:  a
  simple database for typesetting documentation.''  University of
  Washington, Dept. of Statistics, Technical Report."
			  "Steele, G.L., Jr. [1990] ``Common LISP:  The
Language, Second Edition.'' Digital Press."
			  "Tierney, L. [1990] ``LISP-STAT:  An Object
  Oriented Environment for Statistical COmputing and Dynamic
  Graphics''.  John Wiley and Sons."
)
	:copyright "This work is public domain.  The authors make no
warantee as to its suitability for any particular purpose."
	:change-history (list
			 "Version 1.0.3 Minor cleanups"
			 "Version 1.0.2 Added minimal links and anchors to user
level documentation.  Still a lot to do here."
			 "Version 1.0.2 Added Change Log feature."
			 "Version 1.0.1 Added Project Level
Documenation Objects.")
	:files (list 
		(merge-pathnames "package.lsp" *definitions-directory*)
		(merge-pathnames "exports.lsp" *definitions-directory*)
		(merge-pathnames "defs.lsp" *definitions-directory*)
		(merge-pathnames "definition.lsp" *definitions-directory*)
		(merge-pathnames "pack.lsp" *definitions-directory*)
		(merge-pathnames "global.lsp" *definitions-directory*)
		(merge-pathnames "fun.lsp" *definitions-directory*)
		(merge-pathnames "type.lsp" *definitions-directory*)
		(merge-pathnames "build.lsp" *definitions-directory*)
		(merge-pathnames "read.lsp" *definitions-directory*)
		(merge-pathnames "xls-object.lsp" *definitions-directory*)
		(merge-pathnames "filter.lsp" *definitions-directory*)
		(merge-pathnames "sort.lsp" *definitions-directory*)
		(merge-pathnames "condition.lsp" *definitions-directory*)
		(merge-pathnames "tex-print.lsp" *definitions-directory*)
		(merge-pathnames "html-print.lsp" *definitions-directory*)
		(merge-pathnames "project-doc.lsp" *definitions-directory*)
		(merge-pathnames "tex-print-doc.lsp" *definitions-directory*)
		(merge-pathnames "html-print-doc.lsp" *definitions-directory*)
		)
	)
  "This is the project documentation object for the definitions package.")
  
(df:build-project-definitions *doc*)

;;; Printing Script
;;; Uncomment those lines you want to execute


;; Print project summary, TeX mode:
(df:print-tex-documentation *doc* 
    (merge-pathnames "brief.tex" *definitions-directory*)
    :mode :tex :brief t)
;; Print project description, TeX model:
(df:print-tex-documentation *doc* 
    (merge-pathnames "doc.tex" *definitions-directory*)
    :mode :tex :brief nil)
;; Print user manual, TeX model:
(df:print-tex-documentation *doc* 
    (merge-pathnames "user.tex" *definitions-directory*)
    :mode :tex :brief nil :include-user-manual t) 

;; Print project summary, html mode:
(df:print-html-documentation *doc* 
    (merge-pathnames "brief.html" *definitions-directory*)
    :brief t)
;; Print project description, html model:
(df:print-html-documentation *doc* 
    (merge-pathnames "doc.html" *definitions-directory*)
			     :brief nil)
;; Print user manual, html model:
(df:print-html-documentation *doc* 
    (merge-pathnames "user.html" *definitions-directory*)
    :brief nil :include-user-manual t)


;;; Manual building scripts.

(defparameter *definition-defs*
       (df:read-definitions-from-files (send *doc* :files))
  "All definitions")

(df:print-tex-definitions
 (sort (remove-if-not #'df:exported-definition? *definition-defs*)
       #'df:definition-alpha<)
 (merge-pathnames "reference-manual.tex" *definitions-directory*) :mode :tex)


(df:print-html-definitions
 (sort (remove-if-not #'df:exported-definition? *definition-defs*)
       #'df:definition-alpha<)
 (merge-pathnames "reference-manual.html" *definitions-directory*))





	