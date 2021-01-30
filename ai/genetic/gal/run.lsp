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
;  File: run.lisp					    *
;************************************************************

(in-package user)
(provide "run")

(proclaim '(special *evals* *best*))

; Top level function.

(defun run (population-size number-of-bits file n)
   (let ((port (open file :direction :output)))
 	(init-ga-structures population-size number-of-bits)
	(init-ga-stats number-of-bits)
	(do  ((i 1 (1+ i)))
	     ((> i n) t)
	     (ga-search population-size number-of-bits port)
	     (format t "Experiment #~A~%" i)
	     (format port "Individual ")
	     (show-best-individual port)
	     (format port " found in ~A evaluations with score ~A~%"
			   *evals* *best*)
	     (finish-output port))
	(close port)))
