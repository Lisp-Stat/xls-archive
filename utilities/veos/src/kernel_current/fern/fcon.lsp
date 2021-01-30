;;-----------------------------------------------------------
;; file: fcon.lsp
;;
;; FERN is the Fractal Entity Relativity Node.
;; This file is the control flow compenent of the Fern System.
;;
;; creation: February 28, 1992
;;
;; by Geoffrey P. Coco at the HITLab, Seattle
;;-----------------------------------------------------------

;;-----------------------------------------------------------
;; Copyright (C) 1992  Geoffrey P. Coco,
;; Human Interface Technology Lab, Seattle
;;-----------------------------------------------------------


;;-----------------------------------------------------------
#|

These functions manage fern-entity lisp code flow.  They
compose the program control component of the Fern System and
are collectively termed FCON.

Primarily, the FCON handles local behavior code management.

|#
;;-----------------------------------------------------------



;;-----------------------------------------------------------
;;		    FCON Public Functions
;;-----------------------------------------------------------


;; pass "name" and '(function-call with args)
(defun fcon-add-react-proc (proc-name func)
  (progn
    (fcon-delete-react-proc proc-name)
    (setf react-procs (append react-procs (list (list proc-name func))))
    ))

;; pass "name" and '(function-call with args)
(defun fcon-add-persist-proc (proc-name func)
  (progn
    (fcon-delete-persist-proc proc-name)
    (setf persist-procs (append persist-procs (list (list proc-name func))))
    ))
  
;; pass name
(defun fcon-delete-react-proc (proc-name)
  (setq react-procs
	(delete proc-name react-procs :test (lambda (x y) (equal x (car y))))))

;; pass name
(defun fcon-delete-persist-proc (proc-name)
  (setq persist-procs
	(delete proc-name persist-procs :test (lambda (x y) (equal x (car y))))))

;;-----------------------------------------------------------

(defun fcon-ungo (uid)
  (vthrow (list uid) (list 'fcon-local-ungo)))

;;-----------------------------------------------------------



;;-----------------------------------------------------------
;;		   FCON private functions
;;-----------------------------------------------------------


;;-----------------------------------------------------------

(defun fcon-init ()
  (progn
    (setq persist-procs ()
	  react-procs ())
    t))

;;-----------------------------------------------------------

(defun fcon-persist ()
  (progn
    ;; update physical boundary from hardware
    (fph-perceive)

    ;; do actual persisting
    (do-procs persist-procs)

    ;; update hardware from physical boundary
    (fph-exude)
    ))

(defun fcon-react ()
  (do-procs react-procs))

;;-----------------------------------------------------------



