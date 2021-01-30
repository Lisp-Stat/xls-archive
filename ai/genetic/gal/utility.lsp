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
;  File: utility.lisp					    *
;************************************************************

(in-package user)
(provide "utility")

(proclaim '(special *T*))

; Keep track of the generation number...

(defun time-keeper ()
   (setq *T* (1+ *T*))
   (format t "~%Generation ~A:~%" *T*)
)

; Return a random integer from 1 to n inclusive.

(defun rand (n)
    (1+ (truncate (* n (/ (float (random 99999)) 100000.0) ) ) )
)

; Return a random real from (0.0 1.0).

(defun srand ()
    (/ (+ 1.0 (random 99998)) 100000.0)
)

; Return a random bit (0 or 1).

(defun brand () (- (rand 2) 1))
