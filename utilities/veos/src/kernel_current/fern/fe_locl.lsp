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
;;	     Show Local Object Space
;;===========================================================

(defun lo-dump ()
  (pprint (fe-copy.int.locl)))

(defun lo-empty ()
  (pprint (fe-xtrct.int.locl)))

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

