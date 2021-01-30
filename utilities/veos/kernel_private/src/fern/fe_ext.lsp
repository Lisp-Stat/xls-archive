;;-----------------------------------------------------------
;; file: fe_ext.lsp
;;
;; FERN is the Fractal Entity Relativity Node.
;; Part of the FE component of the Fern System.
;;
;; creation: March 28, 1992
;;
;; by Geoffrey P. Coco at the HITLab, Seattle
;;-----------------------------------------------------------


;;-----------------------------------------------------------
;; Copyright (C) 1992  Geoffrey P. Coco,
;; Human Interface Technology Lab, Seattle
;;-----------------------------------------------------------


;;===========================================================
;;			  External
;;===========================================================

(defun fe-put.ext (ext)
  (vput ext '((~ "perc"
		 > @
		 @
		 @) **)))

;;-----------------------------------------------------------

(defun fe-copy.ext (&key (test-time nil))
  (car (vcopy '(("perc"
		 > @
		 @
		 @) **)
	      :test-time test-time)))

;;-----------------------------------------------------------

(defun fe-xtrct.ext ()
  (vget '(("perc"
	   (> @@)
	   @
	   @) **)))

;;-----------------------------------------------------------

(defun fe-get.ext ()
  (car (vput "%" '((~ "perc"
		      > @
		      @
		      @) **))))

;;-----------------------------------------------------------



;;===========================================================
;;		    Spaces Sub-Partition
;;===========================================================

;; returns old space-list
(defun fe-put.ext.sps (sps)
  (car (vput sps '((~ "perc"
		      (> @ @2)
		      @2) **))))

;;-----------------------------------------------------------

(defun fe-copy.ext.sps (&key (test-time nil))
  (car (vcopy '(("perc"
		 (> @ @2)
		 @2) **)
	      :test-time test-time)))

;;-----------------------------------------------------------

(defun fe-xtrct.ext.sps ()
  (vget '(("perc"
	   ((> @@) @2)
	   @2) **)))

;;-----------------------------------------------------------

(defun fe-get.ext.sps ()
  (car (vput "%" '((~ "perc"
		      (> @ @2)
		      @2) **))))

;;-----------------------------------------------------------


;;===========================================================
;;		       Spaces Entities
;;===========================================================

;; an ent is (uid data)
(defun fe-jam.ext.sps.ent (ent)
  (vput ent '((~ "perc"
		 ((^ @@) @2)
		 @2) **)))

;;-----------------------------------------------------------

;; an ent is (uid data)
(defun fe-put.ext.sps.ent (ent)
  (cond
   ;; assume the entity already exists, swap in new one
   ((car (vput ent `((~ "perc"
			((> (,(car ent) @) **) @2)
			@2) **))))

   ;; entity didn' exist, insert new ent
   ((fe-jam.ext.sps.ent ent))))

;;-----------------------------------------------------------

(defun fe-copy.ext.sps.ent (uid &key (test-time nil))
  (car (vcopy `(("perc"
		 ((> (,uid @) **) @2)
		 @2) **)
	      :test-time test-time)))

;;-----------------------------------------------------------

(defun fe-xtrct.ext.sps.ent (uid)
  (car (vget `(("perc"
		((> (,uid @) **) @2)
		@2) **))))

;;-----------------------------------------------------------

(defun fe-get.ext.sps.ent (uid)
  (car (vput "%" `((~ "perc"
		      (((~ ,uid > @) **) @2)
		      @2) **))))

;;-----------------------------------------------------------



;;===========================================================
;;		   Siblings Sub-Partition
;;===========================================================

;; returns old sib-list
(defun fe-put.ext.sibs (sibs)
  (car (vput sibs '((~ "perc"
		       (@ > @ @)
		       @2) **))))

;;-----------------------------------------------------------

(defun fe-copy.ext.sibs (&key (test-time nil))
  (car (vcopy '(("perc"
		 (@ > @ @)
		 @2) **)
	      :test-time test-time)))

;;-----------------------------------------------------------

(defun fe-xtrct.ext.sibs ()
  (vget '(("perc"
	   (@ (> @@) @)
	   @2) **)))

;;-----------------------------------------------------------

(defun fe-get.ext.sibs ()
  (car (vput "%" '((~ "perc"
		      (@ > @ @)
		      @2) **))))

;;-----------------------------------------------------------



;;===========================================================
;;		      Siblings Entities
;;===========================================================

(defun fe-jam.ext.sibs.ent (ent)
  (vput ent '((~ "perc"
		 (@ (^ @@) @)
		 @2) **)))
   
;;-----------------------------------------------------------

;; sibling entities are in the form: (uid (virtual object list))
(defun fe-put.ext.sibs.ent (ent)
  (cond
   ;; assume the ent exists, swap in new ent
   ((car (vput ent `((~ "perc"
			(@ (> (,(car ent) @) **) @)
			@2) **))))
   ;; the ent didn't exist, add new ent
   ((fe-jam.ext.sibs.ent ent))
   ))

;;-----------------------------------------------------------

(defun fe-copy.ext.sibs.ent (uid &key (test-time nil))
  (car (vcopy `(("perc"
		 (@ (> (,uid @) **) @)
		 @2) **)
	      :test-time test-time)))

;;-----------------------------------------------------------

(defun fe-xtrct.ext.ents.ent (uid)
  (car (vget `(("perc"
		(@ (> (,uid @) **) @)
		@2) **))))

;;-----------------------------------------------------------

(defun fe-get.ext.ents.ent (uid)
  (car (vput "%" `((~ "perc"
		      (@ ((~ ,uid > @) **) @)
		      @2) **))))

;;-----------------------------------------------------------



;;===========================================================
;;		 Siblings Entities - Complex
;;===========================================================

;; returns list of all external sibs' uids
(defun fe-copy.ext.sibs.uids ()
  (vcopy '(("perc"
	    (@ ((> @ @) **) @)
	    @2) **)
	 :freq "all"))

;;-----------------------------------------------------------




;;===========================================================
;;		  Sibling Entities Objects
;;===========================================================

(defun fe-jam.ext.sibs.ent.ob (uid ob)
  (cond

   ;; assume entity exists, insert new object
   ((vput ob `((~ "perc"
		  (@ ((~ ,uid (^ @@)) **) @)
		  @2) **)))

   ;; entity wasn't there, insert new entity with new object
   ((fe-jam.ext.sibs.ent `(,uid (,ob))))
   ))
   
;;-----------------------------------------------------------

;; ob is a normal object structure: (name (attr-list))
(defun fe-put.ext.sibs.ent.ob (uid ob)
  (cond

   ;; assume entity and object exist, swap in new object
   ((car (vput ob `((~ "perc"
		       (@ ((~ ,uid (> (,(car ob) @) **)) **) @)
		       @2) **))))
   
   ;; object wasn't there, assume entity exists, insert new object
   ((fe-jam.ext.sibs.ent.ob uid ob))
   ))
   
;;-----------------------------------------------------------

(defun fe-copy.ext.sibs.ent.ob (uid ob-name &key (test-time nil))
  (car (vcopy `(("perc"
		 (@ ((,uid (> (,ob-name @) **)) **) @)
		 @2) **)
	      :test-time test-time)))

;;-----------------------------------------------------------

(defun fe-xtrct.ext.sibs.ent.ob (uid ob-name)
  (car (vget `(("perc"
		(@ ((,uid (> (,ob-name @) **)) **) @)
		@2) **))))

;;-----------------------------------------------------------

(defun fe-get.ext.sibs.ent.ob (uid ob-name)
  (car (vput "%" `((~ "perc"
		      (@ ((~ ,uid ((~ ,ob-name > @) **)) **) @)
		      @2) **))))

;;-----------------------------------------------------------



;;===========================================================
;;	     Sibling Entities Objects - Complex
;;===========================================================

;; pass uid, get list of it's ob names
(defun fe-copy.ext.sibs.ent.ob.names (uid)
  (vcopy `(("perc"
	    (@ ((,uid ((> @ @) **)) **) @)
	    @2) **)
	 :freq "all"))

;;-----------------------------------------------------------



;;===========================================================
;;	     Sibling Entities Objects Attributes
;;===========================================================


(defun fe-jam.ext.sibs.ent.ob.attr (uid ob-name attr)
  (cond
   ;; assume entity and ob exists, insert new attr
   ((vput attr `((~ "perc"
		  (@
		   ((~ ,uid ((~ ,ob-name (^ @@)) **)) **)
		   @)
		  @2) **)))
  
   ;; ob wasn't there, insert new ob with new attr
   ((fe-jam.ext.sibs.ent.ob uid `(,ob-name (,attr))))
   ))

;;-----------------------------------------------------------

;; attr is ("attr-name" attr-val)
(defun fe-put.ext.sibs.ent.ob.attr (uid ob-name attr)
  (cond
   ;; assume the ent, ob and attr exist, swap in new attr
   ((car (vput attr `((~ "perc"
			 (@ 
			  ((~ ,uid ((~ ,ob-name (> (,(car attr) @) **)) **)) **)
			  @)
			 @2) **))))

   ;; attr wasn't there, insert new attr
   ((fe-jam.ext.sibs.ent.ob.attr uid ob-name attr))
   ))
   
;;-----------------------------------------------------------

;; pass uid, ob-num, attr-name
(defun fe-copy.ext.sibs.ent.ob.attr (uid ob-num attr-name &key (test-time nil))
  (car (vcopy `(("perc"
		 (@
		  ((,uid ((,ob-num (> (,attr-name @) **)) **)) **)
		  @)
		 @2) **)
	      :test-time test-time)))

;;-----------------------------------------------------------

;; pass uid, ob-num, attr-name
(defun fe-xtrct.ext.sibs.ent.ob.attr (uid ob-num attr-name)
  (car (vget `(("perc"
		(@
		 ((,uid ((,ob-num (> (,attr-name @) **)) **)) **)
		 @)
		@2) **))))

;;-----------------------------------------------------------

;; pass uid, ob-num, attr-name
(defun fe-get.ext.sibs.ent.ob.attr (uid ob-num attr-name)
  (car (vput "%" `((~ "perc"
		    (@
		     ((~ ,uid ((~ ,ob-num ((~ ,attr-name > @) **)) **)) **)
		     @)
		    @2) **))))

;;-----------------------------------------------------------


;;===========================================================
;;	Sibling Entities Objects Attributes - Complex
;;===========================================================

;; pass uid and ob, return attr-list
(defun fe-copy.ext.sibs.ent.ob.attr.names (uid ob-name)
  (vcopy `(("perc"
	    (@
	     ((,uid ((,ob-name ((> @ @) **)) **)) **)
	     @)
	    @2) **)
	 :freq "all"))

;;-----------------------------------------------------------

;; pass attr, return values of all objects of all sibs
(defun fe-copy.ext.sibs.attr.vals (attr-name)
  (vcopy `(("perc"
	    (@
	     ((@ ((@ ((,attr-name > @) **)) **)) **)
	     @)
	    @2) **)
	 :freq "all"))

;;-----------------------------------------------------------

;; pass uid, ob-num, attr-name
(defun fe-copy.ext.sibs.ent.ob.attr.val (uid ob-num attr-name)
  (car (vcopy `(("perc"
		 (@
		  ((,uid ((,ob-num ((,attr-name > @) **)) **)) **)
		  @)
		 @2) **))))

;;-----------------------------------------------------------




;;===========================================================
;;		    Filters Sub-Partition
;;===========================================================

;; filters are ("attr" (inclusion-list))
(defun fe-put.ext.fltrs (fltrs)
  (vput fltrs '((~ "perc"
		   (@2 > @)
		   @2) **)))

;;-----------------------------------------------------------

(defun fe-copy.ext.fltrs (&key (test-time nil))
  (car (vcopy '(("perc"
		 (@2 > @)
		 @2) **)
	      :test-time test-time)))

;;-----------------------------------------------------------

(defun fe-xtrct.ext.fltrs ()
  (vget '(("perc"
	   (@2 (> @@))
	   @2) **)))

;;-----------------------------------------------------------

(defun fe-get.ext.fltrs ()
  (car (vput "%" '((~ "perc"
		      (@2 > @)
		      @2) **))))

;;-----------------------------------------------------------
