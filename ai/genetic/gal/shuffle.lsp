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
;  File: shuffle.lisp					    *
;************************************************************

(in-package user)
(provide "shuffle")

(proclaim '(special *S* *SH* *B* *C* *O*))

; Shuffle the shuffle array - in order to get a new order
; for the clones in the new population. The shuffle array
; should have been set from the selection function. The
; goal is to have no position dependencies.

(defun shuffle ()
    (do ((i 1 (1+ i))) ((> i *S*) t) (swap i (rand *S*)))
)

(defun swap (i j &aux temp)
   (setq temp (aref *SH* i))
   (setf (aref *SH* i) (aref *SH* j))
   (setf (aref *SH* j) temp)
)
 
; Copy the old population to the new based on the shuffle array.

(defun copy-population ()
   (do ((i 1 (1+ i))) ((> i *S*) t) (copy-individual (aref *SH* i) i))
)
  
; Copies individual i in the old population to j in the new population.
; Must make sure to copy the 0 array location now, since that maintains
; useful evaluation information.

(defun copy-individual (i j)
    (do ((x 0 (1+ x)))
	((> x *B*) t)
	(setf (aref *C* j x) (aref *O* i x)))
)
