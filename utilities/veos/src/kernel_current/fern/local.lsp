;
; local.lsp
;
; Copyright (C) 1992  Washington Technology Center
;
; by Andrew MacDonald at the HITLab
;
; object caching in the local workspace
;
; this is based on fe_bnd.lsp and fe_int.lsp, and manipulates objects
; in perc.int.locl 
;
; functions are of the form fe-(put|get|copy).int.locl.(accessors),
; with macros of the form lo-(put|get|copy).(accessors) defined
; for each function
;
;;-----------------------------------------------------------
;; file: fe.lsp
;; by Geoffrey P. Coco at the HITLab, Seattle
;;-----------------------------------------------------------

;;-----------------------------------------------------------
;; Copyright (C) 1992  Human Interface Technology Lab, Seattle
;;-----------------------------------------------------------


;;===========================================================
;;		       Local Objects
;;===========================================================

(defun fe-jam.int.locl.ob (ob)
  (vput ob
	'(("perc"
	   @2
	   ((^ @@) @2)) **)))
  
;;-----------------------------------------------------------

;; objects are (ob-name (attr-list))
(defun fe-put.int.locl.ob (ob)
  (cond

   ;; assume object is already there
   ((car (vput ob `(("perc"
		     @2
		     ((> (,(car ob) @) **) @2)) **))))

   ;; object wasn't there, insert new one
   ((fe-jam.int.locl.ob ob))
   ))

;;-----------------------------------------------------------

;; pass object name
(defun fe-copy.int.locl.ob (ob-name)
  (car (vcopy `(("perc"
		 @2
		 ((> (,ob-name @) **) @2)) **))))

;;-----------------------------------------------------------

;; pass object name, returns entire object
(defun fe-get.int.locl.ob (ob-name)
  (car (vget `(("perc"
		@2
		((> (,ob-name @) **) @2)) **))))

;;-----------------------------------------------------------



;;===========================================================
;;		  Local Object - Complex
;;===========================================================

(defun fe-copy.int.locl.ob.names ()
  (vcopy `(("perc"
	    @2
	    (((> @ @) **) @2)) **)
	 :freq "all"))

;;-----------------------------------------------------------




;;===========================================================
;;		  Local Object Attributes
;;===========================================================

(defun fe-jam.int.locl.ob.attr (ob-name attr)
  (cond
   ;; assume object exists, add new attr
   ((vput attr `(("perc"
		  @2
		  (((,ob-name (^ @@)) **) @2)) **)))

   ;; object didn't exist, add new object with new attr.
   ((fe-jam.int.locl.ob `(,ob-name (,attr))))
   ))

;;-----------------------------------------------------------

(defun fe-put.int.locl.ob.attr (ob-name attr)
  (cond

   ;; assume the object and attr exist, swap in new attr
   ((car (vput attr `(("perc"
		       @2
		       (((,ob-name (> (,(car attr) @) **)) **) @2)) **))))
    
   ;; attr didn't exist, add new attr
   ((fe-jam.int.locl.ob.attr ob-name attr))
   ))

;;-----------------------------------------------------------

(defun fe-get.int.locl.ob.attr (ob-name attr-name)
  (car (vget `(("perc"
		@2
		(((,ob-name (> (,attr-name @) **)) **) @2)) **))))

;;-----------------------------------------------------------

;; returns attr struct
(defun fe-copy.int.locl.ob.attr (ob-name attr-name)
  (car (vcopy `(("perc"
		 @2
		 (((,ob-name (> (,attr-name @) **)) **) @2)) **))))
  
;;-----------------------------------------------------------



;;===========================================================
;;	     Local Object Attributes - Complex
;;===========================================================

;; returns list of boundary attribute names
(defun fe-copy.int.locl.ob.attr.names (ob-name)
  (vcopy `(("perc"
	    @2
	    (((,ob-name ((> @ @) **)) **) @2)) **)
	 :freq "all"))

;;-----------------------------------------------------------

;; returns attr val
(defun fe-copy.int.locl.ob.attr.val (ob-name attr-name)
  (car (vcopy `(("perc"
		 @2
		 (((,ob-name ((,attr-name > @) **)) **) @2)) **))))
  
;;===========================================================
;;	     Show Local Object Space
;;===========================================================

(defun lo-dump ()
  (pprint (fe-copy.int.locl)))

(defun lo-empty ()
  (pprint (fe-get.int.locl)))

;;===========================================================
;;	     Macro Shortcuts
;;===========================================================

(defmacro lo-jam-ob (ob)
  `(fe-jam.int.locl.ob ,ob))

(defmacro lo-put-ob (ob)
  `(fe-put.int.locl.ob ,ob))

(defmacro lo-copy-ob (ob-name)
  `(fe-copy.int.locl.ob ,ob-name))

(defmacro lo-get-ob (ob-name)
  `(fe-get.int.locl.ob ,ob-name))

;----------------------------------------------------------------

(defmacro lo-copy-ob-names ()
  '(fe-copy.int.locl.ob.names))

;----------------------------------------------------------------

(defmacro lo-jab-attr (ob-name attr)
  `(fe-jam.int.locl.ob.attr ,ob-name ,attr))

(defmacro lo-put-attr (ob-name attr)
  `(fe-put.int.locl.ob.attr ,ob-name ,attr))

;----------------------------------------------------------------

(defmacro lo-get-attr (ob-name attr-name)
  `(fe-get.int.locl.ob.attr ,ob-name ,attr-name))

;----------------------------------------------------------------

(defmacro lo-copy-attr (ob-name attr-name)
  `(fe-copy.int.locl.ob.attr ,ob-name ,attr-name))

(defmacro lo-copy-attr-names (ob-name)
  `(fe-copy.int.locl.ob.attr.names ,ob-name))

(defmacro lo-copy-attr-val (ob-name attr-name)
  `(fe-copy.int.locl.ob.attr.val ,ob-name ,attr-name))

