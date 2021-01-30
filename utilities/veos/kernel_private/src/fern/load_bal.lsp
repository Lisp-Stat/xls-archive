;;-----------------------------------------------------------
;; file: fbal.lsp
;;
;; FERN is the Fractal Entity Relativity Node.
;; This file is the Load Balancing module of the Fern System.
;;
;; creation: February 28, 1992
;;
;; by Geoffrey P. Coco at the HITLab, Seattle
;;-----------------------------------------------------------

;;-----------------------------------------------------------
;; Copyright (C) 1992  Human Interface Technology Lab, Seattle
;;-----------------------------------------------------------


;;-----------------------------------------------------------
#|

These functions provide transparent load-balancing for Fern
Entities and compose the Load Balancing (or FBAL) component
of the Fern System.

The FBAL component ensures that the various computational
requirements of a running scenario are evenly distributed
across the local area network workstations.

Simply, entity computational requirements are rated with a
scalar, as are hardware capabilities.  Also important are the
hardware peripheral requirements an entity.

These factors are rules for inferencing on which local
network host to place the next entity.  The one local-area
FBAL entity (the universe, by convention) maintains the
dynamic database of network-node/entity-load information.

No dynamic balancing is projected (i.e. migrating entities).

|#
;;-----------------------------------------------------------



;;-----------------------------------------------------------
;;		   FBAL Private Functions
;;-----------------------------------------------------------

(define fbal-init ()
  (progn
    (vget '(> ("balance" @@) **))
    (vput '("balance" () ()) '(^ @@))
    
    ))


(defun fbal-make-node (binary program)
  (vthrow (list universe) `(fbal-remote-new-make-node binary
						      program
						      ,self))
  )


(defun fbal-remote-make-node (binary program ancestor)
  (fcon-make-node (fbal-avail-host binary program) binary program ancestor)
  )


(defun fbal-avail-host (binary program)
  
  )




