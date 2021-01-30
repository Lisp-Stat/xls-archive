;;; -*- Mode:LISP; Base:10; Syntax:Common-Lisp; -*-

;************************************************************
;                                                           *
;  William M. Spears					    *
;  Navy Center for Applied Research in AI                   *
;  Naval Research Laboratory                                *
;                                                           *
;  This software is the property of the Department of the   *
;  Navy. Permission is hereby granted to copy all or any    *
;  part of this program for free distribution, however      *
;  this header is required on all copies.		    *
;                                                           *
;  File: compile-ga.lisp				    *
;************************************************************

(in-package user)
(provide "compile-ga")
(proclaim '(optimize (safety 0) (space 0) (compilation-speed 0) (speed 3)))


; **********************************************************************; 
; 									; 
; 	The following must be changed (or added to) if this file	; 
; 	is used with a new node, or new Lisp.           		; 
; 									; 
; **********************************************************************; 

; THE NODE

#+(and Lucid Sun)(defvar *node-prefix* "./")

; TYPE OF LISP - SOURCE SUFFIXES AND BINARY SUFFIXES

#+(and Lucid Sun)(defvar *node-source-suffix* ".lisp")

#+(and Lucid Sun)(defvar *node-binary-suffix* ".sbin") 

#+xlisp (defvar *node-source-suffix* ".lsp")
#+xlisp (defvar *node-binary-suffix* ".fsl")
#+xlisp (defvar *node-prefix* "./")

; --------------------------------------------------------------

; Top level function to compile GAL.

(defun compile-ga ()
   (mapc 
     #'(lambda (file)
	(let ((node-source (file-format *node-prefix* file
					*node-source-suffix*))
	      (node-binary (file-format *node-prefix* file
					*node-binary-suffix*)))

	     (cond
		((probe-file node-binary) nil)
		(t (compile-file node-source)))

	     (load  node-binary)
	))

	`(
	  "term" "utility" "cross" "fitness" "geval" "global" "mutate"
	  "select" "shuffle" "ga" "conv" "myeval"
	  ))
)

; Create the file string.

(defun file-format (prefix file suffix)
   (format nil "~A~A~A" prefix file suffix)
)
