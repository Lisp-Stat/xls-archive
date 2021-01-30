;;-----------------------------------------------------------
;; file: fern.lsp
;;
;; FERN is the Fractal Entity Relativity Node.
;; This file is the controller of the FERN compenents
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
;;			 Fern System
;;-----------------------------------------------------------
#|

Fern is a distributed world information management system.
Fern provides the transparent underpinnings for distributed
world data maintenance.

Fern maintains a "perc" (e.g. perception) partition in an
entity's grouplespace (see below).  Fern transparently
updates the "perc" partition of an entity's local
grouplespace to contain all world data relevant to the
entity.

("perc"

    (;ext
  
    	(;sps (;ent))
   
    	(;sibs (;ent (;ob (;attr))))

    	(;fltrs)
    )
    (;bndry

    	(;vrt (;ob (;attr)))
   
    	(;phys (;ob (;attr)))
    )
    (;int

    	(;subs (;ent (;ob (;attr))))

	(;fltrs (;ent))

    	(;locl (;ob (;attr)))
    )
)

The "perc" partition is accessable through fe- functions.
Use fe- functions by composing the partition names you want
to access.  For example, if you want to change an attribute
in the virtual boundary, use (fe-put.bndry.vrt.ob.attr)

|#
;;-----------------------------------------------------------



;;-----------------------------------------------------------
;;		     Fern Initialization
;;-----------------------------------------------------------


(defun fern-init ()
  (progn

    ;;;
    ;;; init the VEOS kernel
    ;;; watch out for previous initialization
    ;;;

    (let (zoot)
      (cond ((setq zoot (vinit))
	     (setq self zoot))))

    ;;;
    ;;; other initial accounting
    ;;;

    (setq fern-debug t)


    ;;;
    ;;; initialize Fern System C module
    ;;;

    (fbase-init)


    ;;;
    ;;; load and initialize Fern System lisp modules
    ;;;

    (load "fgod")
    (fgod-init)

    (load "fe")
    (fe-init)

    (load "fx")
    (fx-init)

    (load "fcon")
    (fcon-init)

    (load "fph")
    (fph-init)


    ;;;
    ;;; print fern header
    ;;;

    (fern-credits)
    t))

;;-----------------------------------------------------------



;;-----------------------------------------------------------
;;			  Utilities
;;-----------------------------------------------------------


;;-----------------------------------------------------------

(defun dump ()
  (pprint (vcopy '(> @@))))

(defun empty ()
  (pprint (vget '(> @@))))

(defmacro pp (expr) (pprint (eval expr)))

;;-----------------------------------------------------------

(defun uid2str (uid)
  (sprintf (aref uid 0) " " (aref uid 1)))

(defun str2uid (str)
  (let ((lst (sscanf str)))
    (vector (car lst) (cadr lst))))

;;-----------------------------------------------------------

(defun vect2list (vect)
  (do ((ret NIL)
       (index (1- (length vect))))

      ((eq index -1)
       ret)

      (setq ret (cons (aref vect index) ret))
      (setq index (1- index))
      ))

;;-----------------------------------------------------------

(defun list2vect (lst)
  (do* ((len (length lst))
	(ret (make-array len))
	(index 0))

       ((eq index len)
	ret)

       (setf (aref ret index) (car lst))
       (setq index (1+ index))
       (setq lst (cdr lst))
       ))

;;-----------------------------------------------------------

;; a partition looks like ("name" (everything))
(defun put-gspace-partition (new-part)
  (cond 
   ;; assume partition is already there
   ((vput new-part `(> (,(car new-part) @@) **)))

   ;; partition wasn't there, insert new
   ((vput new-part '(^ @@)))))

;; part name is a string
(defun copy-gspace-partition (part-name)
  (car (vcopy `(> (,part-name @@) **))))

;; part name is a string
(defun get-gspace-partition (part-name)
  (car (vget `(> (,part-name @@) **))))

;;-----------------------------------------------------------

(defun do-procs (pro-list)
  (dolist (proc pro-list)
	  (eval (cadr proc))))

;;-----------------------------------------------------------




;;-----------------------------------------------------------
;;		 Main Fern Private Functions
;;-----------------------------------------------------------


;;-----------------------------------------------------------
(defun fern-credits ()
  (printf "


		 ``````````````````````````
		   The Fern System v1.0b1
                        by Geoff Coco
		  Copyright (C) 1992, HITL

		 ''''''''''''''''''''''''''
"))

;;-----------------------------------------------------------





;;-----------------------------------------------------------
;;		    Invoke Initialization
;;-----------------------------------------------------------

(fern-init)





