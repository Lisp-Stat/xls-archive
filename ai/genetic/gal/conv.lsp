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
;  File: conv.lisp					    *
;************************************************************

(in-package user)
(provide "conv")

(proclaim '(special *best* *terms* *evals* *S* *C* *B* *conv* *T*))

(setq *conv* .89)		; The convergence factor.
				; Changed to .89 from .9 because
				; one example only has 10 bits.

; Return t iff *conv* percent of the columns have converged.

(defun convergence? (port)
   (do	((j 1 (1+ j)) (temp 0))
	((> j *B*)
;		(convergence-report temp port)
		(if (> temp (* *conv* *B*)) t nil))
	(if (column-convergence j) (setq temp (1+ temp)))))

; Return t iff the column is converged to one value by *conv* percent.

(defun column-convergence (j)
   (do	((i 1 (1+ i)) (temp 0))
	((> i *S*) (if (or (> temp (* *conv* *S*))
			   (< temp (* (- 1.0 *conv*) *S*))) t nil))
	(if (= 1 (aref *C* i j)) (setq temp (1+ temp)))))

(defun convergence-report (conv port)
   (format port "Generation ~A: Convergence = ~A, Best = ~A~%"
	*T* (/ (float conv) *B*) *best*)
)
