;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: :cl-User;-*-
;;;  File Name          :  system.lisp
;;;  Version Number     :  1.7
;;;  Last Changed Date  :  95/11/14 At 16:49:13
;;;  Created Date       :  11/14/95
;;;
;;; Copyright 1991. John Alan McDonald. All Rights Reserved. 
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

;;; 6/12/98 RGA --- Modified to work with XLS
;;;============================================================

(in-package :User)

#|
;;;============================================================
;;;  Find the directory this file is being loaded from.
;;;  Borrowed from PCL.

(eval-when (compile load eval)
  (unless (fboundp 'load-truename)

(defun load-truename (&optional (errorp nil))
  (flet ((bad-time ()
	   (when errorp
	     (error "LOAD-TRUENAME called but a file isn't being loaded."))))
    #+Lispm  (or sys:fdefine-file-pathname (bad-time))
    #+excl   excl::*source-pathname*
    #+Xerox  (pathname (or (il:fullname *standard-input*) (bad-time)))
    #+(and dec vax common) (truename (sys::source-file #'load-truename))
    ;;
    ;; The following use of  `lucid::' is a kludge for 2.1 and 3.0
    ;; compatibility.  In 2.1 it was in the SYSTEM package, and i
    ;; 3.0 it's in the LUCID-COMMON-LISP package.
    ;;
    #+LUCID (or lucid::*source-pathname* (bad-time))))

(defun load-directory ()
  (make-pathname :directory (pathname-directory (load-truename))))
)

(defparameter *definitions-directory* (namestring (load-directory)))

;(defparameter *definitions-bin-directory*
;    (concatenate 'String
;      #.*definitions-directory*
;      #+allegro "bin/franz/"
;      #+cmu "bin/cmu/"))
;
;)

;;;============================================================
;;; make sure Kantrowitz's defsystem (make) package is loaded

;(eval-when (compile load eval)
;  (unless (find-package :Make)
;    (load (merge-pathnames "defsystem" *defsystem-directory*))))

|#
;;;============================================================

;(mk:defsystem :Definitions
;    :source-pathname #.*definitions-directory*
;    :source-extension "lisp"
;    :binary-pathname #.*definitions-bin-directory*
;    :binary-extension nil
;    :components ("package"
;		 "exports"
;		 "defs"
;		 "definition"
;		 "pack"
;		 "global"
;		 "fun"
;		 "type"
;		 "build"
;		 "read"
;		 "filter"
;		 "sort"
;		 ;"latex-print"
;		 "tex-print"
;		 "condition"
;		 #+kr-doc
;		 "schema"
;		 #+announcements
;		 "announcements"
;		 #+gb+api
;		 "gb-object"
;		 ))


(defun load-directory ()
  (make-pathname :directory (pathname-directory *load-truename*))
  )

(defparameter *definitions-directory* (namestring (load-directory)))
(defparameter *xlos-loader*
  (let ((dir (pathname-directory *load-truename*)))
    (setf (car (last dir)) "xlos")
    (namestring 
     (make-pathname :directory dir :name "system"))))



(defparameter *definition-tex-header-file*
    (merge-pathnames "lispdef.tex" *definitions-directory*)
  "Filename for TeX Header File.")

(defparameter *definition-latex-header-file*
    (merge-pathnames "lispdefl.tex" *definitions-directory*)
  "Filename for LaTeX Header File.")

;;; I'm not going to try to use defsystem, I'm just going to
;;; load one file at a time.

(require "XLOS" *xlos-loader*)

(load (merge-pathnames "package.lsp" *definitions-directory*))
(load (merge-pathnames "exports.lsp" *definitions-directory*))
(load (merge-pathnames "defs.lsp" *definitions-directory*))
(load (merge-pathnames "definition.lsp" *definitions-directory*))
(load (merge-pathnames "pack.lsp" *definitions-directory*))
(load (merge-pathnames "global.lsp" *definitions-directory*))
(load (merge-pathnames "fun.lsp" *definitions-directory*))
(load (merge-pathnames "type.lsp" *definitions-directory*))
(load (merge-pathnames "build.lsp" *definitions-directory*))
(load (merge-pathnames "read.lsp" *definitions-directory*))
(load (merge-pathnames "xls-object.lsp" *definitions-directory*))
(load (merge-pathnames "filter.lsp" *definitions-directory*))
(load (merge-pathnames "sort.lsp" *definitions-directory*))
(load (merge-pathnames "condition.lsp" *definitions-directory*))
(load (merge-pathnames "tex-print.lsp" *definitions-directory*))
(load (merge-pathnames "html-print.lsp" *definitions-directory*))
(load (merge-pathnames "project-doc.lsp" *definitions-directory*))
(load (merge-pathnames "tex-print-doc.lsp" *definitions-directory*))
(load (merge-pathnames "html-print-doc.lsp" *definitions-directory*))

(provide "DEFINITIONS")
