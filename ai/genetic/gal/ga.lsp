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
;  File: ga.lisp					    *
;************************************************************

(in-package user)
(require "utility")
(require "cross")
(require "fitness")
(require "geval")
(require "global")
(require "mutate")
(require "select")
(require "shuffle")
(require "term")
(require "conv")
(provide "ga")

(proclaim '(special *C* *F* *SH* *done*))

; Run GA repeatedly until you are done.

(defun ga-search (pop-size bits port)
   (setq *done* nil)
   (init-ga-variables pop-size bits .001 .6)
   (init-ga-population)

   (do	((i 1 (1+ i)))
	(*done* t)
	(format t "Iteration ~A of GA at ~A generations~%" i *T*)
	(format port "Iteration ~A of GA at ~A generations~%" i *T*)
	(run-ga pop-size bits port)
	(if (null *done*) (reinit-everything)))
)

; Your fairly standard generational GA.

(defun run-ga (pop-size bits port)
    (time-keeper)
    (fitness)
    (offspring)
    (do ((i 0 (1+ i)))
	((or (setq *done* (termination?)) (convergence? port)) t)
	(ga-select)
	(shuffle)
	(swap-pops)
	(copy-population)
	(mutate)
	(cross-population)
	(time-keeper)
	(fitness)
	(offspring)
))
