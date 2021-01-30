;;-----------------------------------------------------------
;; file: fph.lsp
;;
;; FERN is the Fractal Entity Relativity Node.
;; This file is the FPH compenent of the Fern System.
;;
;; creation: March 28, 1992
;;
;; by Geoffrey P. Coco at the HITLab, Seattle
;;-----------------------------------------------------------

;;-----------------------------------------------------------
;; Copyright (C) 1992  Geoffrey P. Coco,
;; Human Interface Technology Lab, Seattle
;;-----------------------------------------------------------


;;-----------------------------------------------------------
#|

These functions provide users of the Fern System with a
standardized mechanism for coupling hardware-specific
interface code to objects in the 'physical' boundary.  They
represent the FPH component of the Fern System.

|#
;;-----------------------------------------------------------


;;===========================================================
;;
;;		     FPH PUBLIC FUNCTIONS
;;
;;===========================================================

;; pass "name" and '(function-call with args)
(defun fph-add-input-proc (proc-name func)
  (progn
    (fph-delete-input-proc proc-name)
    (setf pre-persist-procs (append pre-persist-procs (list (list proc-name func))))
    ))

;; pass "name" and '(function-call with args)
(defun fph-add-output-proc (proc-name func)
  (progn
    (fph-delete-output-proc proc-name)
    (setf post-persist-procs (append post-persist-procs (list (list proc-name func))))
    ))
  
;; pass name
(defun fph-delete-input-proc (proc-name)
  (setq pre-persist-procs
	(delete proc-name pre-persist-procs :test (lambda (x y) (equal x (car y))))))

;; pass name
(defun fph-delete-output-proc (proc-name)
  (setq post-persist-procs
	(delete proc-name post-persist-procs :test (lambda (x y) (equal x (car y))))))


;;===========================================================
;;
;;		     FPH PRIVATE FUNCTIONS
;;
;;===========================================================


;;-----------------------------------------------------------

(defun fph-init ()
  (setq pre-persist-procs ()
	post-persist-procs ())
  )

;;-----------------------------------------------------------

;; update physical boundary from hardware
(defun fph-perceive ()
  (do-procs pre-persist-procs)
  )

;; update hardware from physical boundary
(defun fph-exude ()
  (do-procs post-persist-procs)
  )
;;-----------------------------------------------------------



