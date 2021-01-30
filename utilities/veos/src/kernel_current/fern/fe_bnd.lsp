;;-----------------------------------------------------------
;; file: fe_bnd.lsp
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
;;			  Boundary
;;===========================================================

(defun fe-put.bndry (bndry)
  (vput bndry '((~ "perc"
		   @
		   > @
		   @) **)))

;;-----------------------------------------------------------

(defun fe-copy.bndry (&key (test-time nil))
  (car (vcopy '(("perc"
		 @
		 > @
		 @) **)
	      :test-time test-time)))

;;-----------------------------------------------------------

(defun fe-xtrct.bndry ()
  (vget '(("perc"
	   @
	   (> @@)
	   @) **)))

;;-----------------------------------------------------------

(defun fe-get.bndry ()
  (car (vput "%" '((~ "perc"
		      @
		      > @
		      @) **))))

;;-----------------------------------------------------------



;;===========================================================
;;			   Virtual
;;===========================================================

;; returns old virtual bndry
(defun fe-put.bndry.vrt (vbndry)
  (car (vput vbndry '((~ "perc"
			 @
			 (@ > @ @)
			 @) **))))

;;-----------------------------------------------------------

;; cache this frequently used pattern in C level fern.
;; later, calls to fe-copy.bndry.vrt use precomputed pattern.

(fbase-init-copy.bndry.vrt '(("perc"
			      @
			      (@ > @ @)
			      @) **))

#|
(defun fe-copy.bndry.vrt (&key (test-time nil))
  (car (vcopy '(("perc"
		 @
		 (@ > @ @)
		 @) **)
	      :test-time test-time)))
|#
;;-----------------------------------------------------------

(defun fe-xtrct.bndry.vrt ()
  (vget '(("perc"
	   @
	   (@ (> @@) @)
	   @) **)))

;;-----------------------------------------------------------

(defun fe-get.bndry.vrt ()
  (car (vput "%" '(("perc"
		    @
		    (@ > @ @)
		    @) **))))

;;-----------------------------------------------------------



;;===========================================================
;;		       Virtual Objects
;;===========================================================

(defun fe-jam.bndry.vrt.ob (ob)
  (vput ob '((~ "perc"
		@
		(@ (^ @@) @)
		@) **)))

;;-----------------------------------------------------------

;; objects are (ob-name (attr-list))
(defun fe-put.bndry.vrt.ob (ob)
  (cond

   ;; assume object is already there
   ((car (vput ob `((~ "perc"
		       @
		       (@ (> (,(car ob) @) **) @)
		       @) **))))

   ;; object wasn't there, insert new one
   ((fe-jam.bndry.vrt.ob ob))
   ))

;;-----------------------------------------------------------

;; pass object name
(defun fe-copy.bndry.vrt.ob (ob-name &key (test-time nil))
  (car (vcopy `(("perc"
		 @
		 (@ (> (,ob-name @) **) @)
		 @) **)
	      :test-time test-time)))

;;-----------------------------------------------------------

(defun fe-xtrct.bndry.vrt.ob (ob-name)
  (car (vget `(("perc"
		@
		(@ (> (,ob-name @) **) @)
		@) **))))

;;-----------------------------------------------------------

(defun fe-get.bndry.vrt.ob (ob-name)
  (car (vput "%" `((~ "perc"
		      @
		      (@ ((~ ,ob-name > @) **) @)
		      @) **))))

;;-----------------------------------------------------------



;;===========================================================
;;		  Virtual Object - Complex
;;===========================================================

(defun fe-copy.bndry.vrt.ob.names ()
  (vcopy `(("perc"
	    @
	    (@ ((> @ @) **) @)
	    @) **)
	 :freq "all"))

;;-----------------------------------------------------------




;;===========================================================
;;		  Virtual Object Attributes
;;===========================================================

(defun fe-jam.bndry.vrt.ob.attr (ob-name attr)
  (cond
   ;; assume object exists, add new attr
   ((vput attr `((~ "perc"
		    @
		    (@ ((~ ,ob-name (^ @@)) **) @)
		    @) **)))
   
   ;; object didn't exist, add new object with new attr.
   ((fe-jam.bndry.vrt.ob `(,ob-name (,attr))))
   ))

;;-----------------------------------------------------------

(defun fe-put.bndry.vrt.ob.attr (ob-name attr)
  (cond

   ;; assume the object and attr exist, swap in new attr
   ((car (vput attr `((~ "perc"
			 @
			 (@ ((~ ,ob-name (> (,(car attr) @) **)) **) @)
			 @) **))))
   
   ;; attr didn't exist, add new attr
   ((fe-jam.bndry.vrt.ob.attr ob-name attr))
   ))

;;-----------------------------------------------------------

(defun fe-xtrct.bndry.vrt.ob.attr (ob-name attr-name)
  (car (vget `(("perc"
		@
		(@ ((,ob-name (> (,attr-name @) **)) **) @)
		@) **))))

;;-----------------------------------------------------------

(defun fe-get.bndry.vrt.ob.attr (ob-name attr-name)
  (car (vput "%" `((~ "perc"
		      @
		      (@ ((~ ,ob-name ((~ ,attr-name > @) **)) **) @)
		      @) **))))

;;-----------------------------------------------------------

;; returns attr struct
(defun fe-copy.bndry.vrt.ob.attr (ob-name attr-name &key (test-time nil))
  (car (vcopy `(("perc"
		 @
		 (@ ((,ob-name (> (,attr-name @) **)) **) @)
		 @) **)
	      :test-time test-time)))
  
;;-----------------------------------------------------------



;;===========================================================
;;	     Virtual Object Attributes - Complex
;;===========================================================

;; returns list of boundary attribute names
(defun fe-copy.bndry.vrt.ob.attr.names (ob-name)
  (vcopy `(("perc"
	    @
	    (@ ((,ob-name ((> @ @) **)) **) @)
	    @) **)
	 :freq "all"))

;;-----------------------------------------------------------

;; returns attr val
(defun fe-copy.bndry.vrt.ob.attr.val (ob-name attr-name)
  (car (vcopy `(("perc"
		 @
		 (@ ((,ob-name ((,attr-name > @) **)) **) @)
		 @) **))))
  
;;-----------------------------------------------------------




;;===========================================================
;;		    Physical Sub-Partition
;;===========================================================

;; returns old physical bndry
(defun fe-put.bndry.phys (vbndry)
  (car (vput vbndry '((~ "perc"
			 @
			 (@2 > @)
			 @) **))))

;;-----------------------------------------------------------

(defun fe-copy.bndry.phys (&key (test-time nil))
  (car (vcopy '(("perc"
		 @
		 (@2 > @)
		 @) **)
	      :test-time test-time)))

;;-----------------------------------------------------------

(defun fe-xtrct.bndry.phys ()
  (vget '(("perc"
	   @
	   (@2 (> @@))
	   @) **)))

;;-----------------------------------------------------------

(defun fe-get.bndry.phys ()
  (car (vput "%" '((~ "perc"
		      @
		      (@2 > @)
		      @) **))))

;;-----------------------------------------------------------



;;===========================================================
;;		       Physical Objects
;;===========================================================

(defun fe-jam.bndry.phys.ob (ob)
  (vput ob '((~ "perc"
		@
		(@2 (^ @@))
		@) **)))
  
;;-----------------------------------------------------------

;; objects are (ob-name (attr-list))
(defun fe-put.bndry.phys.ob (ob)
  (cond

   ;; assume object is already there
   ((car (vput ob `((~ "perc"
		       @
		       (@2 (> (,(car ob) @) **))
		       @) **))))

   ;; object wasn't there, insert new one
   ((fe-jam.bndry.phys.ob ob))
   ))

;;-----------------------------------------------------------

;; pass object name
(defun fe-copy.bndry.phys.ob (ob-name &key (test-time nil))
  (car (vcopy `(("perc"
		 @
		 (@2 (> (,ob-name @) **))
		 @) **)
	      :test-time test-time)))

;;-----------------------------------------------------------

(defun fe-xtrct.bndry.phys.ob (ob-name)
  (car (vget `(("perc"
		@
		(@2 (> (,ob-name @) **))
		@) **))))

;;-----------------------------------------------------------

(defun fe-get.bndry.phys.ob (ob-name)
  (car (vput "%" `((~ "perc"
		      @
		      (@2 ((~ ,ob-name > @) **))
		      @) **))))

;;-----------------------------------------------------------




;;===========================================================
;;		  Physical Object - Complex
;;===========================================================

(defun fe-copy.bndry.phys.ob.names ()
  (vcopy `(("perc"
	    @
	    (@2 ((> @ @) **))
	    @) **)
	 :freq "all"))

;;-----------------------------------------------------------




;;===========================================================
;;		  Physical Object Attributes
;;===========================================================

(defun fe-jam.bndry.phys.ob.attr (ob-name attr)
  (cond
   ;; assume object exists, add new attr
   ((vput attr `((~ "perc"
		    @
		    (@2 ((~ ,ob-name (^ @@)) **))
		    @) **)))

   ;; object didn't exist, add new object with new attr.
   ((fe-jam.bndry.phys.ob `(,ob-name (,attr))))
   ))

;;-----------------------------------------------------------

(defun fe-put.bndry.phys.ob.attr (ob-name attr)
  (cond

   ;; assume the object and attr exist, swap in new attr
   ((car (vput attr `((~ "perc"
			 @
			 (@2 ((~ ,ob-name (> (,(car attr) @) **)) **))
			 @) **))))
   
   ;; attr didn't exist, add new attr
   ((fe-jam.bndry.phys.ob.attr ob-name attr))
   ))

;;-----------------------------------------------------------

(defun fe-xtrct.bndry.phys.ob.attr (ob-name attr-name)
  (car (vget `(("perc"
		@
		(@2 ((,ob-name (> (,attr-name @) **)) **))
		@) **))))

;;-----------------------------------------------------------

(defun fe-get.bndry.phys.ob.attr (ob-name attr-name)
  (car (vput "%" `((~ "perc"
		      @
		      (@2 ((~ ,ob-name ((~ ,attr-name > @) **)) **))
		      @) **))))

;;-----------------------------------------------------------

;; returns attr struct
(defun fe-copy.bndry.phys.ob.attr (ob-name attr-name &key (test-time nil))
  (car (vcopy `(("perc"
		 @
		 (@2 ((,ob-name (> (,attr-name @) **)) **))
		 @) **)
	      :test-time test-time)))
  
;;-----------------------------------------------------------



;;===========================================================
;;	     Physical Object Attributes - Complex
;;===========================================================

;; returns list of boundary attribute names
(defun fe-copy.bndry.phys.ob.attr.names (ob-name)
  (vcopy `(("perc"
	    @
	    (@2 ((,ob-name ((> @ @) **)) **))
	    @) **)
	 :freq "all"))

;;-----------------------------------------------------------

;; returns attr val
(defun fe-copy.bndry.phys.ob.attr.val (ob-name attr-name)
  (car (vcopy `(("perc"
		 @
		 (@2 ((,ob-name ((,attr-name > @) **)) **))
		 @) **))))
  
;;-----------------------------------------------------------




