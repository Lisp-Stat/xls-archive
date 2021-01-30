;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: :cl-User;-*-
;;;  File Name          :  system.lisp
;;;  Version Number     :  1.7
;;;  Last Changed Date  :  95/11/14 At 16:49:13
;;;  Created Date       :  11/14/95
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

(in-package :User)
(defun load-directory ()
  (make-pathname :directory (pathname-directory *load-truename*))
  )

(defparameter *xlos-directory* (namestring (load-directory)))

(load (merge-pathnames "defclass.lsp" *xlos-directory*))
(load (merge-pathnames "defgeneric.lsp" *xlos-directory*))
(provide "XLOS")
