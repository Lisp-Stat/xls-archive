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
;  File: cross.lisp					    *
;************************************************************

(in-package user)
(provide "cross")

(proclaim '(special *cr* *S* *B* *C* *cr-stats* *cross-change*))

; Cross-over a percentage of the population, based on the
; cross-over rate and the population size.

(defun cross-population ()
    (do ((i 1 (+ 2 i))) ((> i (* *cr* *S*)) t) (cross i (1+ i)))
)

; Do cross-over of two individuals. The following diagram should help.

; 		1	2	3					*B*
; 	+---------------------------------------------------------------+
;  i:	|   1	|   2	|   3	|	|	|	|	|  *B*	|
; 	+---------------------------------------------------------------+

; 		1	2	3					*B*
; 	+---------------------------------------------------------------+
;  j:	|   1	|   2	|   3	|	|	|	|	|  *B*	|
; 	+---------------------------------------------------------------+

(defun cross (i j)
  (let* ((x (rand *B*))
	 (y (rand *B*))
	 (x1 (min x y))		; smaller cross-over point.
	 (y1 (max x y)))	; larger cross-over point.
	(or (= x1  y1) (cross-over i j x1 y1))
)  )

; Either One or Two point cross-over. Mark for re-evaluation if
; a change has occurred..

(defun cross-over (i j x1 y1)
    (setq *cross-change* nil)
    (do ((x (1+ x1) (1+ x))) ((> x y1) t) (cross-swap i j x))
    (cond
	(*cross-change*
	    (setf (aref *C* i 0) nil)	; re-evaluate.
	    (setf (aref *C* j 0) nil)))	; re-evaluate.
)

; Swap one bit position in two individuals.

(defun cross-swap (i j x)
   (let ((tempi (aref *C* i x))
	 (tempj (aref *C* j x)))
	(cond
	    ((eq tempi tempj) t)
	    (t	(setq *cross-change* t)
		(setf (aref *C* i x) tempj)
		(setf (aref *C* j x) tempi)))
)  )
