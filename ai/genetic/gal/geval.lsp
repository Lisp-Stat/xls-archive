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
;  File: geval.lisp					    *
;************************************************************

(in-package user)
(provide "geval")

(proclaim '(special *C* *B* *best* *best-individual* *evals* *storage*))

(setq *storage* nil)

; Code for evaluation of an individual in the current population.
; Keep track of the best one seen so far. This version takes advantage
; of previously computed evaluations from the prior generation. If
; a flag is true, there is no need to re-evaluate the individual.
; Mutation and cross-over will set the flag to false, in which case
; re-evaluation must occur. Also, keep track of the number of
; evaluations that occur. This is a better indication of the
; the number of individuals searched (it would be better to maintain
; all individuals, but that would be space intensive!).

(defun geval (i)
  (let  ((value (aref *C* i 0)))
	(cond
	     (value value)
	     (t ;(format t "Re-evaluating individual ~A~%" i)
		(setq value (float (myeval i)) *evals* (1+ *evals*))
		(setf (aref *C* i 0) value)
		(and (> value *best*)
		     (setq *best* value)
		     (store-best-individual i))
		value))
))

(defun store-best-individual (i)
    (do ((j 1 (1+ j)))
	((> j *B*) t)
	(setf	(aref *best-individual* j)
		(aref *C* i j))
)   )
