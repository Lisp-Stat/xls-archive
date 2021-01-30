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

;;; This file is template of the project documentation features.  Fill
;;; out this form to complete the documentation for your project.
;;; Below is a script for running it.

;;; For an example, see the file doc-doc.lsp

(defparameter *doc*
  (send df:project-doc-proto :new
	:title ""
	:authors 
	(list (send df:author-doc-proto :new :name "" :affiliation ""
		    :email "@" :home-page "http://")
	      )
	:version 
	(send df:version-doc-proto :new :major 1 :minor 0  :rev-level 0
	      :date (multiple-value-bind (sec min hr day mn yr dow
					      dstp tz)
			(get-decoded-time) 
			(format nil "~D/~D/~D" yr mn day)))
	:package ""
	:nicknames (list )
	:package-dependencies (list "LISP")
	:motivation ""
	:functional-description ""
	:stat-description ""
	:instructions ""
	:examples (list "")
	:xls-version "No known incompatabilities"
	:os-dependencies "No known incompatabilities"
	:warnings (list "")
	:see-also (list "")
	:references (list "")
	:copyright "This work is public domain.  The authors make no
warantee as to its suitability for any particular purpose."
	:files (list "doc.lsp")
	)
  "This is the Documenation Object for this project")

(df:build-project-definitions *doc*)


;;; Printing Script
;;; Uncomment those lines you want to execute
;;; Note:  This script assumes that the definition package has
;;; previously been loaded.

;; Print project summary, TeX mode:
;(df:print-tex-documentation *doc* "brief.tex" :mode :tex :brief t)
;; Print project description, TeX model:
;(df:print-tex-documentation *doc* "doc.tex" :mode :tex :brief nil)
;; Print user manual, TeX model:
;(df:print-tex-documentation *doc* "user.tex" :mode :tex :brief nil
;			    :include-user-manual t)

;; Print project summary, html mode:
;(df:print-html-documentation *doc* "brief.html" :brief t)
;; Print project description, html model:
;(df:print-html-documentation *doc* "doc.html" :brief nil)
;; Print user manual, html model:
;(df:print-html-documentation *doc* "user.html" :brief nil
;			    :include-user-manual t)







	