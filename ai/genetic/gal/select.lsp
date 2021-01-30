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
;  File: select.lisp					    *
;************************************************************

(in-package user)
(require "utility")
(provide "select")

(proclaim '(special *F* *SH* *S*))

; Essentially Baker's SUS algorithm.

(defun ga-select ()
  (prog (i r count)
	(setq i 1 r (srand) count 1)
	loop
	(cond
	    ((< r (aref *F* i))
		(setf (aref *SH* count) i)
		(setq r (+ 1 r) count (1+ count))
		(go loop))
	    ((<= count *S*)
		(setq i (1+ i))
		(go loop))
	    (t t))
)  )
	
