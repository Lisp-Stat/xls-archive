;;-----------------------------------------------------------
;; file: fe_int.lsp
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
;;			  Internal
;;===========================================================

(defun fe-put.int (int)
  (vput int '((~ "perc"
		 @
		 @
		 > @) **)))

;;-----------------------------------------------------------

(defun fe-copy.int (&key (test-time nil))
  (car (vcopy '(("perc"
		 @
		 @
		 > @) **)
	      :test-time test-time)))

;;-----------------------------------------------------------

(defun fe-xtrct.int ()
  (vget '(("perc"
	   @
	   @
	   (> @@) **))))

;;-----------------------------------------------------------

(defun fe-get.int ()
  (car (vput "%" '((~ "perc"
		      @
		      @
		      > @) **))))

;;-----------------------------------------------------------




;;-----------------------------------------------------------
;; The following functions which manipulate the locl
;; sub-partition were composed by Andy MacDonald
;;-----------------------------------------------------------


;;===========================================================
;;			    Local
;;===========================================================

(defun fe-put.int.locl (locl)
  (vput locl '((~ "perc"
		  @2
		  (> @ @2)) **)))

;;-----------------------------------------------------------

(defun fe-copy.int.locl (&key (test-time nil))
  (car (vcopy '(("perc"
		 @2
		 (> @ @2)) **)
	      :test-time test-time)))

;;-----------------------------------------------------------

(defun fe-xtrct.int.locl ()
  (vget '(("perc"
	   @2
	   ((> @@) @2)) **)))

;;-----------------------------------------------------------

(defun fe-get.int.locl ()
  (car (vput '((~ "perc"
		  @2
		  (> @ @2)) **))))

;;-----------------------------------------------------------



;;===========================================================
;;		       Local Objects
;;===========================================================

(defun fe-jam.int.locl.ob (ob)
  (vput ob '((~ "perc"
		@2
		((^ @@) @2)) **)))
  
;;-----------------------------------------------------------

;; objects are (ob-name (attr-list))
(defun fe-put.int.locl.ob (ob)
  (cond

   ;; assume object is already there
   ((car (vput ob `((~ "perc"
		       @2
		       ((> (,(car ob) @) **) @2)) **))))

   ;; object wasn't there, insert new one
   ((fe-jam.int.locl.ob ob))
   ))

;;-----------------------------------------------------------

;; pass object name
(defun fe-copy.int.locl.ob (ob-name &key (test-time nil))
  (car (vcopy `(("perc"
		 @2
		 ((> (,ob-name @) **) @2)) **)
	      :test-time test-time)))

;;-----------------------------------------------------------

;; pass object name, returns entire object
(defun fe-xtrct.int.locl.ob (ob-name)
  (car (vget `(("perc"
		@2
		((> (,ob-name @) **) @2)) **))))

;;-----------------------------------------------------------

(defun fe-get.int.locl.ob (ob-name)
  (car (vput "%" `((~ "perc"
		      @2
		      (((~ ,ob-name > @) **) @2)) **))))

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
   ((vput attr `((~ "perc"
		    @2
		    (((~ ,ob-name (^ @@)) **) @2)) **)))

   ;; object didn't exist, add new object with new attr.
   ((fe-jam.int.locl.ob `(,ob-name (,attr))))
   ))

;;-----------------------------------------------------------

(defun fe-put.int.locl.ob.attr (ob-name attr)
  (cond
   
   ;; assume the object and attr exist, swap in new attr
   ((car (vput attr `((~ "perc"
			 @2
			 (((~ ,ob-name (> (,(car attr) @) **)) **) @2)) **))))
    
   ;; attr didn't exist, add new attr
   ((fe-jam.int.locl.ob.attr ob-name attr))
   ))

;;-----------------------------------------------------------

(defun fe-xtrct.int.locl.ob.attr (ob-name attr-name)
  (car (vget `(("perc"
		@2
		(((,ob-name (> (,attr-name @) **)) **) @2)) **))))

;;-----------------------------------------------------------

(defun fe-get.int.locl.ob.attr (ob-name attr-name)
  (car (vput "%" `((~ "perc"
		      @2
		      (((~ ,ob-name ((~ ,attr-name > @) **)) **) @2)) **))))

;;-----------------------------------------------------------

;; returns attr struct
(defun fe-copy.int.locl.ob.attr (ob-name attr-name &key (test-time nil))
  (car (vcopy `(("perc"
		 @2
		 (((,ob-name (> (,attr-name @) **)) **) @2)) **)
	      :test-time test-time)))
  
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
  
;;-----------------------------------------------------------



;;===========================================================
;;			  Sublings
;;===========================================================

(defun fe-put.int.subs (subs)
  (vput subs '((~ "perc"
		  @2
		  (@ > @ @)) **)))

;;-----------------------------------------------------------

;; cache this frequently used pattern in C level fern.
;; later, calls to fe-copy.int.subs use precomputed pattern.

(fbase-init-copy.int.subs '(("perc"
			     @2
			     (@ > @ @)) **))

#|
(defun fe-copy.int.subs (&key (test-time nil))
  (car (vcopy '(("perc"
		 @2
		 (@ > @ @)) **)
	      :test-time test-time)))
|#
;;-----------------------------------------------------------

(defun fe-xtrct.int.subs ()
  (vget '(("perc"
	   @2
	   (@ (> @@) @)) **)))

;;-----------------------------------------------------------

(defun fe-get.int.subs ()
  (car (vput "%" '((~ "perc"
		      @2
		      (@ > @ @)) **))))

;;-----------------------------------------------------------


;;===========================================================
;;		      Sublings Entities
;;===========================================================

(defun fe-jam.int.subs.ent (ent)
  (vput ent '((~ "perc"
		 @2
		 (@ (^ @@) @)) **)))

;;-----------------------------------------------------------

;; an ent is: (uid (ob-list))
(defun fe-put.int.subs.ent (ent)
  (cond

   ;; assume the ent exists, swap in the new ent
   ((car (vput ent `((~ "perc"
			@2
			(@ (> (,(car ent) @) **) @)
			) **))))

   ;; ent didn't exist, insert new ent
   ((fe-jam.int.subs.ent ent))
   ))
		      
;;-----------------------------------------------------------

(defun fe-copy.int.subs.ent (uid &key (test-time nil))
  (car (vcopy `(("perc"
		 @2
		 (@ (> (,uid @) **) @)
		 ) **)
	      :test-time test-time)))

;;-----------------------------------------------------------

(defun fe-xtrct.int.subs.ent (uid)
  (car (vget `(("perc"
		@2
		(@ (> (,uid @) **) @)
		) **))))

;;-----------------------------------------------------------

(defun fe-get.int.subs.ent (uid)
  (car (vput "%" `((~ "perc"
		      @2
		      (@ ((~ ,uid > @) **) @)
		      ) **))))

;;-----------------------------------------------------------



;;===========================================================
;;		 Sublings Entities - Complex
;;===========================================================

(defun fe-copy.int.subs.uids ()
  (vcopy '(("perc"
	    @2
	    (@ ((> @ @) **) @)
	    ) **)
	 :freq "all"))

;;-----------------------------------------------------------




;;===========================================================
;;		  Sublings Entities Objects
;;===========================================================


(defun fe-jam.int.subs.ent.ob (uid ob)
  (cond

   ;; assume entity exists, insert new object
   ((vput ob `((~ "perc"
		  @2
		  (@ ((~ ,uid (^ @@)) **) @)
		  ) **)))

   ;; entity wasn't there, insert new entity with new object
   ((fe-jam.int.subs.ent `(,uid (,ob))))
   ))
   
;;-----------------------------------------------------------

;; ob is a normal object structure: (name (attr-list))
(defun fe-put.int.subs.ent.ob (uid ob)
  (cond

   ;; assume entity and object exist, swap in new object
   ((car (vput ob `((~ "perc"
		       @2		       
		       (@ ((~ ,uid (> (,(car ob) @) **)) **) @)
		       ) **))))
   
   ;; object wasn't there, assume entity exists, insert new object
   ((fe-jam.int.subs.ent.ob uid ob))
   ))
   
;;-----------------------------------------------------------

(defun fe-copy.int.subs.ent.ob (uid ob-name &key (test-time nil))
  (car (vcopy `(("perc"
		 @2
		 (@ ((,uid (> (,ob-name @) **)) **) @)
		 ) **)
	      :test-time test-time)))

;;-----------------------------------------------------------

(defun fe-xtrct.int.subs.ent.ob (uid ob-name)
  (car (vget `(("perc"
		@2
		(@ ((,uid (> (,ob-name @) **)) **) @)
		) **))))

;;-----------------------------------------------------------

(defun fe-get.int.subs.ent.ob (uid ob-name)
  (car (vput "%" `((~ "perc"
		      @2
		      (@ ((~ ,uid ((~ ,ob-name > @) **)) **) @)
		      ) **))))

;;-----------------------------------------------------------



;;===========================================================
;;	     Subling Entities Objects - Complex
;;===========================================================

;; pass uid, get list of it's ob names
(defun fe-copy.int.subs.ent.ob.names (uid)
  (vcopy `(("perc"
	    @2
	    (@ ((,uid ((> @ @) **)) **) @)
	    ) **)
	 :freq "all"))

;;-----------------------------------------------------------




;;===========================================================
;;	     Subling Entities Objects Attributes
;;===========================================================


(defun fe-jam.int.subs.ent.ob.attr (uid ob-name attr)
  (cond
   ;; assume entity and ob exists, insert new attr
   ((vput attr `((~ "perc"
		    @2
		    (@
		     ((~ ,uid ((~ ,ob-name (^ @@)) **)) **)
		     @)
		    ) **)))
  
   ;; ob wasn't there, insert new ob with new attr
   ((fe-jam.int.subs.ent.ob uid `(,ob-name (,attr))))
   ))

;;-----------------------------------------------------------

;; attr is ("attr-name" attr-val)
(defun fe-put.int.subs.ent.ob.attr (uid ob-name attr)
  (cond
   ;; assume the ent, ob and attr exist, swap in new attr
   ((car (vput attr `((~ "perc"
			 @2
			 (@ 
			  ((~ ,uid ((~ ,ob-name (> (,(car attr) @) **)) **)) **)
			  @)
			 ) **))))

   ;; attr wasn't there, insert new attr
   ((fe-jam.int.subs.ent.ob.attr uid ob-name attr))
   ))
   
;;-----------------------------------------------------------

;; pass uid, ob-num, attr-name
(defun fe-copy.int.subs.ent.ob.attr (uid ob-num attr-name &key (test-time nil))
  (car (vcopy `(("perc"
		 @2
		 (@
		  ((,uid ((,ob-num (> (,attr-name @) **)) **)) **)
		  @)
		 ) **)
	      :test-time test-time)))

;;-----------------------------------------------------------

;; pass uid, ob-num, attr-name
(defun fe-xtrct.int.subs.ent.ob.attr (uid ob-num attr-name)
  (car (vget `(("perc"
		@2
		(@
		 ((,uid ((,ob-num (> (,attr-name @) **)) **)) **)
		 @)
		) **))))

;;-----------------------------------------------------------

;; pass uid, ob-num, attr-name
(defun fe-get.int.subs.ent.ob.attr (uid ob-num attr-name)
  (car (vput "%" `((~ "perc"
		      @2
		      (@
		       ((~ ,uid ((~ ,ob-num ((~ ,attr-name > @) **)) **)) **)
		       @)
		      ) **))))

;;-----------------------------------------------------------



;;===========================================================
;;	Subling Entities Objects Attributes - Complex
;;===========================================================

;; pass uid and ob, return attr-list
(defun fe-copy.int.subs.ent.ob.attr.names (uid ob-name)
  (vcopy `(("perc"
	    @2
	    (@
	     ((,uid ((,ob-name ((> @ @) **)) **)) **)
	     @)
	    ) **)
	 :freq "all"))

;;-----------------------------------------------------------

;; pass attr, return values of all objects of all sibs
(defun fe-copy.int.subs.attr.vals (attr-name)
  (vcopy `(("perc"
	    @2
	    (@
	     ((@ ((@ ((,attr-name > @) **)) **)) **)
	     @)
	    ) **)
	 :freq "all"))

;;-----------------------------------------------------------

;; pass uid, ob-num, attr-name
(defun fe-copy.int.subs.ent.ob.attr.val (uid ob-num attr-name)
  (car (vcopy `(("perc"
		 @2
		 (@
		  ((,uid ((,ob-num ((,attr-name > @) **)) **)) **)
		  @)
		 ) **))))

;;-----------------------------------------------------------





;;===========================================================
;;			   Filters
;;===========================================================

(defun fe-put.int.fltrs (fltr)
  (vput fltr '((~ "perc"
		  @2
		  (@2 > @)) **)))

;;-----------------------------------------------------------

(defun fe-copy.int.fltrs (&key (test-time nil))
  (car (vcopy '(("perc"
		 @2
		 (@2 > @)) **)
	      :test-time test-time)))

;;-----------------------------------------------------------

(defun fe-xtrct.int.fltrs ()
  (vget '(("perc"
	   @2
	   (@2 (> @@))) **)))

;;-----------------------------------------------------------

(defun fe-get.int.fltrs ()
  (car (vput "%" '((~ "perc"
		      @2
		      (@2 > @)) **))))

;;-----------------------------------------------------------



;;===========================================================
;;		       Fltrs Entities
;;===========================================================

(defun fe-jam.int.fltrs.ent (ent)
  (vput ent '((~ "perc"
		 @2
		 (@2 (^ @@))) **)))

;;-----------------------------------------------------------

;; an ent is: (uid (ob-list))
(defun fe-put.int.fltrs.ent (ent)
  (cond

   ;; assume the ent exists, swap in the new ent
   ((car (vput ent `((~ "perc"
			@2
			(@2 (> (,(car ent) @) **))
			) **))))

   ;; ent didn't exist, insert new ent
   ((fe-jam.int.fltrs.ent ent))
   ))
		      
;;-----------------------------------------------------------

(defun fe-copy.int.fltrs.ent (uid &key (test-time nil))
  (car (vcopy `(("perc"
		 @2
		 (@2 (> (,uid @) **))
		 ) **)
	      :test-time test-time)))

;;-----------------------------------------------------------

(defun fe-xtrct.int.fltrs.ent (uid)
  (car (vget `(("perc"
		@2
		(@2 (> (,uid @) **))
		) **))))

;;-----------------------------------------------------------

(defun fe-get.int.fltrs.ent (uid)
  (car (vput "%" `((~ "perc"
		      @2
		      (@2 ((~ ,uid > @) **))
		      ) **))))

;;-----------------------------------------------------------




;;===========================================================
;;	      Internal Entity Filter Processing
;;===========================================================


;;-----------------------------------------------------------

(defun fe-fltr.int.subs (uid &key (test-time nil))
  (delete uid
	  (fe-copy.int.subs :test-time test-time)
	  :test (lambda (x y) (equal x (car y)))))
  
;;-----------------------------------------------------------

(defun fe-fltr.int.subs.uids (uid)
  (delete uid 
	  (fe-copy.int.subs.uids)
	  :test 'equal))
  
;;-----------------------------------------------------------




