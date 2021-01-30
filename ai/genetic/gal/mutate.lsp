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
;  File: mutate.lisp					    *
;************************************************************

(in-package user)
(require "utility")
(provide "mutate")

(proclaim '(special *bit* *bits* *B* *C* *m*))

; Do mutation.
; *bit* = marks the picked bit NOT mutated in the last generation.
; new	= new position to mutate if possible.

; Probably not the best way to do this - but it should work.

(defun mutate ()
   (let ((total-bits (* *T* *bits*)))
	(cond
	     ((< *bit* total-bits)
			(flip-bit (mod *bit* *bits*))
			(do ((new (+ *bit* (jump)) (+ new (jump))))
			    ((> new total-bits) (setq *bit* new))
			    (flip-bit (mod new *bits*))))
	     (t nil))
)  )

; Randomly select a bit and flip it. This is different from selecting
; a bit, and then randomly filling that bit location with a 1 or 0!
; The population is considered to be a linear sequence of bits.

(defun flip-bit (bit)
  (let  ((i (+ (truncate (/ bit *B*)) 1))
	 (j (+ (mod bit *B*) 1)))
;	(format t "Mutation at individual ~A, bit ~A.~%" i j)
	(setf (aref *C* i 0) nil)
	(if (zerop (aref *C* i j))
	    (setf (aref *C* i j) 1)
	    (setf (aref *C* i j) 0))
) )

; The actual jump.

(defun jump () (truncate (/ (log (srand)) (log (- 1.0 *m*)))))
