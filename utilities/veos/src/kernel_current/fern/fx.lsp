;;-----------------------------------------------------------
;; file: fx.lsp
;;
;; FERN is the Fractal Entity Relativity Node.
;; This file is the trans compenent of the Fern System.
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

The FX component of the Fern System is concerned with normal
communications between space and entity.

A primary purpose of the Fern System is to transparently
maintain the distributed world database.  These functions
provide the inter-entity transport mechanism for the Fern
System and they compose the FX component of the Fern System.

|#
;;-----------------------------------------------------------



;;===========================================================
;;
;;		     FX PUBLIC FUNCTIONS
;;
;;===========================================================


;;-----------------------------------------------------------
;;	       Entity Level Binding Entry Pts
;;-----------------------------------------------------------


;;-----------------------------------------------------------

;; instigate a space/entity relationship between your entity
;; and the given entity.  sp-uid becomes a space of yours.
;; any entity/space token loop begins here.

(defun fx-enter (sp-uid)
  (cond ((fe-copy.ext.sps.ent sp-uid))
	(t
	 (cond (fern-debug
		(printf "fx-enter... new space: " (uid2str sp-uid))))

	 ;; add new space to external partition 
	 (fe-put.ext.sps.ent (list sp-uid ()))

	 ;; add delta time-stamp to hash table
	 (fbase-put-hash fern-ent-htab sp-uid (vmintime))

	 ;; pass first boundary update (i.e. the token) to space
	 (vthrow (list sp-uid) 
		 `(fx-sp-enter ,self (quote ,(fx-ent-exude sp-uid))))
	 t)))

;;-----------------------------------------------------------

;; instagate the exit procedure.
;; terminate a space/entity relationship between your entity
;; and the given entity.  the token will travel one more time
;; around.	 

(defun fx-exit (sp-uid)


  (cond ((fe-copy.ext.sps.ent sp-uid)

	 (cond (fern-debug
		(printf "fx-exit... space: " (uid2str sp-uid))))

	 ;; remove space from space list.
	 (fe-get.ext.sps.ent sp-uid)

	 ;; remove this entry from timestamp hash table
	 (fbase-get-hash fern-ent-htab sp-uid)

	 ;; assume that entity has the token.
	 ;; NOTE: this is not a safe assumption.
	 ;; In other words, it is only safe to call
	 ;; fx-exit from within a react proc.

	 ;; token now passes one more time through it's loop
	 ;; to the space (fx-sp-exit, fx-sp-unperceive)
	 ;; then, ending at (fx-ent-unperceive)
	 (vthrow (list sp-uid) 
		 `(fx-sp-exit ,self))
	 )))

;;-----------------------------------------------------------



;;===========================================================
;;
;;		     FX PRIVATE FUNCTIONS
;;
;;===========================================================


;;-----------------------------------------------------------

(defun fx-init ()
  (setq fern-sp-htab (fbase-new-htab))
  (setq fern-ent-htab (fbase-new-htab))
  )

;;-----------------------------------------------------------



;;-----------------------------------------------------------
;;		Space Level Binding Entry Pts
;;-----------------------------------------------------------


;;-----------------------------------------------------------

;; accept a space/entity relationship between from another 
;; entity.  setup initial data structures, pass included
;; entity->space token into token loop.

(defun fx-sp-enter (ent-uid vbf)
  (progn
    (cond (fern-debug
	   (printf "fx-sp-enter... new entity: " (uid2str ent-uid))))

    ;; the entering entity has already checked for duplicates.
    ;; add new entity to sublings partition,
    ;; overiding a possible previous deletion.
    (fe-put.int.subs.ent (list ent-uid nil))

    ;; add new entity with delta time-stamp to fern hash table
    (fbase-put-hash fern-sp-htab ent-uid (vmintime))

    ;; pass included token to normal space entry point
    (fx-sp-perceive ent-uid vbf)
    ))

;;-----------------------------------------------------------

;; accept a termination request of space/entity relationship
;; from another entity.  takedown data structures, pass included
;; entity->space token into final token loop.

(defun fx-sp-exit (ent-uid)
  (progn
    (cond (fern-debug
	   (printf "fx-sp-exit... retiring entity: " (uid2str ent-uid))))

    ;; remove entity timestamp hash table
    (fbase-get-hash fern-sp-htab ent-uid)

    ;; remove entity from sublings partition..
    ;; and thus from the externals of all other entities in this space.
    (fe-get.int.subs.ent ent-uid)

    ;; eliminate perceptual references to this entity
    (fx-sp-unperceive ent-uid)
    ))

;;-----------------------------------------------------------



;;-----------------------------------------------------------
;;		Space Token Normal Operation
;;-----------------------------------------------------------


;;-----------------------------------------------------------

(defun fx-ent-perceive (which-sp bndry-list)
  (progn
    (cond (fern-debug
	   (printf "fx-ent-perceive ... from space: " (uid2str which-sp))
	   (printf "the goods:")
	   (pprint bndry-list)))

    ;;;
    ;;; update external siblings partition of local grouplespace
    ;;;

    (dolist (ent bndry-list)

	    (cond
	     ((equal (car ent) self))
	     ((consp (cadr ent))
	      (dolist (ob (cadr ent))
		      
		      (cond
		       ((consp (cadr ob))
			(dolist (attr (cadr ob))

				(fe-put.ext.sibs.ent.ob.attr (car ent) (car ob) attr)))

		       ((fe-put.ext.sibs.ent.ob (car ent) ob)))))

	     ((fe-put.ext.sibs.ent ent))))

    ;; dispatch reactive behaviors
    (do-procs react-procs)
    
    ;; repost to space all instant changes
    ;; this is the bottom of the entity-space token loop
    (vthrow (list which-sp) 
	    `(fx-sp-perceive ,self (quote ,(fx-ent-exude which-sp))))

    (gc)
    ))

;;-----------------------------------------------------------

(defun fx-ent-exude (sp-uid)

  ;; generate a vbf structure to match space protocol.
  ;; a vbf structure is (uid virt-bndry fltr).

  ;; as of 5/1/92, filters are omitted and
  ;; the uid is passed as a separate argument.

  (let* ((ts 0.0) vbf)

    (fbase-hash fern-ent-htab sp-uid ts)
    (setq vbf (fe-copy.bndry.vrt :test-time ts))

    ;; replace time stamp for this space
    (fbase-put-hash fern-ent-htab sp-uid ts)
    
    (cond (fern-debug
	   (printf "fx-ent-exude ... to space: " (uid2str sp-uid))
	   (printf "the goods:")
	   (pprint vbf)))

    ;; return the entity->space token to caller
    vbf))

;;-----------------------------------------------------------



;;-----------------------------------------------------------

(defun fx-sp-perceive (uid vbf)
  (progn
    ;; top of the entity-space token loop.
    
    (cond (fern-debug
	   (printf "fx-sp-perceive ... from entity: " (uid2str uid))
	   (printf "the goods:")
	   (pprint vbf)))
    
    ;;;
    ;;; update internal sublings partition of grouplepsace
    ;;;

    (cond 
     ((listp vbf)
      (dolist (ob vbf) 
	      
	      (cond
	       ((consp (cadr ob))
		(dolist (attr (cadr ob))
			 
			(fe-put.int.subs.ent.ob.attr uid (car ob) attr)))
	       
	       ((fe-put.int.subs.ent.ob uid ob)))))
     
     ((fe-put.int.subs.ent (list uid vbf))))

			 
    ;;;
    ;;; space takes no reactive action...
    ;;;
    (cond (fern-debug
	   (printf "\nthe world:")
	   (pprint (fe-copy.int.subs))
	   (printf "")))
    
    ;; repost to entity all world activity
    (vthrow (list uid)
	    `(fx-ent-perceive ,self (quote ,(fx-sp-exude uid))))
    ))

;;-----------------------------------------------------------

(defun fx-sp-exude (uid)
  (progn
    ;; generate message to match entity protocol:
    ;; list of sibling entities

    (let* ((ts 0.0) sibs)

      (fbase-hash fern-sp-htab uid ts)
      (setq sibs (fe-copy.int.subs :test-time ts))

      ;; reset this entity's time-stamp
      (fbase-put-hash fern-sp-htab uid ts)

      (cond (fern-debug
	     (printf "fx-sp-exude ... to entity: " (uid2str uid))
	     (printf "the goods:")
	     (pprint sibs)))

      ;; return the space->entity token to caller
      sibs)))

;;-----------------------------------------------------------





;;-----------------------------------------------------------
;;		    Token Loop Unbinding
;;-----------------------------------------------------------


;;-----------------------------------------------------------

;; end of line for space/entity token.

(defun fx-ent-unperceive (which-sp ent-list)
  (progn
    (cond (fern-debug
	   (printf "fx-ent-UNperceive ... from space: " (uid2str which-sp))))
    
    ;; update external perception partition of local grouplespace
    ;; so that all that entities from that space are removed.
    (mapcar 'fe-get.ext.sibs.ent ent-list)
    ))

;;-----------------------------------------------------------

(defun fx-sp-unperceive (uid)
  (progn 
    (cond (fern-debug
	   (printf "fx-sp-UNperceive ... from entity: " (uid2str uid))))

    ;; rid subling of any perceptions of this space
    (vthrow (list uid) 
	    `(fx-ent-unperceive ,self (quote ,(fe-copy.int.subs.uids))))
    ))

;;-----------------------------------------------------------


