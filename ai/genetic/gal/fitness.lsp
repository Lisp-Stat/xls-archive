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
;  File: fitness.lisp					    *
;************************************************************

(in-package user)
(provide "fitness")

; This version of the fitness calculation code uses a global
; average of prior fitnesses (produced by the function geval)
; to produce the proper scaling of values. This should work
; better than the current generation minimum.

(proclaim '(special *global-average-fitness* *average-fitness* 
		    *fitness* *B* *C* *F* *S* *T* *best* *best-individual*))

(defun fitness (&aux sum)
    (setq sum 0 *global-average-fitness* (compute-average-fitness))
    (do ((i 1 (1+ i)))
	((> i *S*) (setq *average-fitness* (/ sum *S*)))
	(let ((value (geval i)))
	     (setq *fitness* (+ value *fitness*))
	     (setq value (compute-value value))
	     (setq sum (+ sum value))
	     (setf (aref *F* i) value)))
    (format t "~%The best individual is ")
    (show-best-individual t)
    (format t  " with score: ~A.~% " *best*)
)

(defun compute-average-fitness ()
   (if (= 1 *T*) 0.0 (/ *fitness* (* *S* (1- *T*))))
)

(defun compute-value (value)
   (if	(> (setq value (- value (/ *global-average-fitness* 2.0))) 0.0)
	value
	0.0)
)

; Convert the fitness values in *F* into partial sums of
; expected number of offspring in *F*. Useful for selection.
; Notice that *F*[*S*] is fixed at *S*. Mathematically, this
; should happen if the partial sums are allowed to run all
; the way to the end. However, math errors in Lisp can accumulate
; and actually produce rather large discrepancies (which can
; cause the select algorithm to attempt to access an array 1
; step out of bounds). This should fix that problem - although
; some slight bias may be introduced.

(defun offspring (&aux prior)
    (setq prior (/ (aref *F* 1) *average-fitness*))
    (setf (aref *F* 1) prior)
    (do ((i 2 (1+ i)))
	((= i *S*) (setf (aref *F* *S*) (float *S*)))
	(setf  (aref *F* i)
		(+ (/ (aref *F* i) *average-fitness*) prior))
	(setq prior (aref *F* i))
)   )

(defun show-best-individual (port)
    (do ((i 1 (1+ i)))
	((> i *B*) t)
	(format port "~A" (aref *best-individual* i)))
)
