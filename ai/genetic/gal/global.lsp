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
;  File: global.lisp					    *
;************************************************************

; All global variables and constructs.

(in-package user)
(provide "global")

; Proclaim the global variables and array names.

; *P1*		= a 1-D population array of strings.
; *P2*		= a 1-D population array of strings.
; *C*		= Current population to be evaluated.
; *O*		= Old population to be used to create the new population.
; *S*		= Size of population (a constant).
; *B*		= length of each individual (# of Bits).
; *SH*		= an array[*S*] of individuals to be used for SHuffling.
; *T*		= time tick number.
; *m*		= mutation rate. (m = .001)
; *cr*		= cross-over rate. (cr = .6)
; *bit*		= the picked bit where mutation did NOT occur in
;		  last generation.
; *bits*	= total number of bits (*S* * *B*)
; *F*		= array[*S*] that holds fitness of individuals.
; *fitness*	= mean fitness
; *cr-stats* 	= array[*B*] of cross-over statistics.
; *best*	= best individual score so far.
; *best-individual* = an array[*B*] for the best individual so far.
; *evals*	= number of evaluations done this trial.

(proclaim '(special *P1* *P2* *C* *O* *S* *B*
		    *SH* *T* *m* *cr* *bit*
		    *bits* *F* *fitness* *cr-stats*
		    *best* *best-individual* *evals*))

; Creates all GA array structures.

(defun init-ga-structures (pop-size bits)
  (setq *P1* (make-array `(,(1+ pop-size) ,(1+ bits))))
  (setq *P2* (make-array `(,(1+ pop-size) ,(1+ bits))))
  (setq *SH* (make-array `(,(1+ pop-size))))
  (setq *F* (make-array `(,(1+ pop-size))))
  (setq *cr-stats* (make-array `(,(1+ bits))))
  (setq *best-individual* (make-array `(,(1+ bits))))
)

; Initializes some important GA variables.

(defun init-ga-variables (pop-size bits mutation cross-over)
   (setq  *C* *P1* *O* *P2* *S* pop-size *B* bits *T* 0
	  *m* mutation *cr* cross-over *bit* (jump)
	  *bits* (* *S* *B*) *fitness* 0 *best* 0
	  *evals* 0)
)

; Initializes all GA array structures.

(defun reinit-arrays ()
   (do	((i 1 (1+ i)))
	((> i *S*) t)
	(setf (aref *SH* i) nil)
	(setf (aref *F* i) nil))
   (do	((i 1 (1+ i)))
	((> i *B*) t)
	(setf (aref *cr-stats* i) nil)
	(setf (aref *best-individual* i) nil))
   (do	((i 1 (1+ i)))
	((> i *S*) t)
	(setf (aref *P1* i 0) nil)	; setf value at 0.
	(setf (aref *P2* i 0) nil)	; setf value at 0.
	(do ((j 1 (1+ j)))
	    ((> j *B*) t)
	    (setf (aref *P1* i j) nil)
	    (setf (aref *P2* i j) nil)))
)

; After one attempt to use the GA algorithm, reinitialize everything
; and try again - this code reinitializes everything.

(defun reinit-everything ()
   (reinit-arrays)
   (setq  *C* *P1* *O* *P2* *bit* (jump) *fitness* 0 *best* 0)
   (init-ga-population)
)

; Initialize the population with random bits.

(defun init-ga-population ()
    (format t "Randomly initializing population.~%")
    (do ((i 1 (1+ i)))
	((> i *S*) t)
	(setf (aref *C* i 0) nil)	; setf value at 0.
	(do ((j 1 (1+ j)))
	    ((> j *B*) t)
	    (setf (aref *C* i j) (brand)))
)   )

; Initialize some crossover statistics.

(defun init-ga-stats (bits)
    (format t "Initializing GA Statistics.~%")
    (do ((j 1 (1+ j))) ((> j bits) t) (setf (aref *cr-stats* j) 0))
)

; Some useful prettyprinting code for arrays...

(defun ppp (label arr size)
    (terpri)
    (do ((i 1 (1+ i)))
	((> i size) t)
	(format t "~A[~A] = ~A~%" label i (aref arr i))
)   )

(defun ppp2 (label arr size1 size2)
    (terpri)
    (do ((i 1 (1+ i)))
	((> i size1) t)
	(format t "~A[~A] = " label i)
	(do ((j 1 (1+ j)))
	    ((> j size2) (terpri))
	    (format t "~A" (aref arr i j)))
)   )

; Swap the two populations that *C* and *O* point to...

(defun swap-pops (&aux temp)
   (setq temp *C* *C* *O* *O* temp)
)

