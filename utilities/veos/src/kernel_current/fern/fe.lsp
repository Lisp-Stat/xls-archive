;;-----------------------------------------------------------
;; file: fe.lsp
;;
;; FERN is the Fractal Entity Relativity Node.
;; This file is the entity component of the Fern System.
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

These functions perform all local database operations are
collectively termed the FE.

The FE maintains the "perc" grouplspace partition.  That is,
all data associated with the Fern System is passed through
these functions to the grouplespace.

Internally, the Fern System uses these functions exclusively.
And, in the interest of forward compatibility, Fern
programmers are highly encouraged to use these functions
exclusively to access Fern's "perc" grouplespace partition.

|#
;;-----------------------------------------------------------



;;-----------------------------------------------------------
;;		    Load FE Subcomponents
;;-----------------------------------------------------------

(load "fe_ext")
(load "fe_bnd")
(load "fe_int")
(load "fe_locl")

;;-----------------------------------------------------------




;;===========================================================
;;
;;		    FE PRIVATE FUNCTIONS
;;
;;===========================================================


;;-----------------------------------------------------------
#|
all FE functions assume that the partitions are setup like
this.  That is, for any FE functions to work properly, the
"perc" partition must be fully articulated. 

A full "perc" partition contains the three subpartitions:
"external", "boundary", "internal".  Furthermore, those
subpartitions must contain their respective sub-partitions.

An empty but articulated perception partition looks like:

("perc"

 (;external
  ()      ;spaces
  ()      ;siblings
  ()      ;filters
  )

 (;boundary
  ()      ;virtual
  ()      ;physical
  )

 (;internal
  ()      ;local
  ()      ;sublings
  ()      ;filters
  )

 )

|#

(defun fe-init ()
  (progn
    ;; setup the "perception" grouplespace partition.
    (put-gspace-partition '("perc"
			    ()      ;; external
			    ()      ;; boundary
			    ()      ;; internal
			    ))

    ;; setup "external" sub-partitions.
    (fe-put.ext '(()     ;; current spaces.
		  ()     ;; siblings among all spaces.
		  ()     ;; perceptual filters.
		  ))

    ;; setup "boundary" sub-partitions.
    (fe-put.bndry (list self  ;; self uid.
			()    ;; virtual object-list.
			()    ;; physical object-list.
			))

    ;; setup "internal" sub-partitions.
    (fe-put.int '(()     ;; local memory
		  ()     ;; the space for the entity's internal world
		  ()     ;; the filters for each contained entity
		  ))
    ))

;;-----------------------------------------------------------
