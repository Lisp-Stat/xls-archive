;;; file calckde.lsp, part of the KDE software by F. Udina
;;; jan 94
;;; fast computation of curve estimates
;; methods supported: warping, fft, updating.
;; this file contains also other functions for computing 
;  data-based bandwidth selectors, and some utility functions
;; Contains also information about kernel functions

(require "binning")

(defvar *kde*computing*inspect* t)
;

(defmacro WHILE (cond &rest exprs)
  `(loop
   (unless ,cond (return))
    (progn ,@exprs)))

(defmacro increment (sym)
  `(setf ,sym (1+ ,sym)))

(defmacro decrement (sym)
  `(setf ,sym (1- ,sym)))

(defmacro showbinding (sym)
  `(format t "~a is ~a~%" (string ',sym) ,sym))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;kernel estimation computing functions
;; must be rewritten to optimize function calling, and for better style...

(defun calc-kernel-density-warping (data width type xvals method want-pairs kdeobj)
  (let* ((nbins (length xvals))
	 (xmin (first xvals))
	 (delta (- (second xvals) xmin))
	 (xmax (+ xmin (* delta (1+ nbins))))
	 (weights (send kdeobj :kernel-weights type 
			delta width (send kdeobj :use-canonical)))
	 (lw (length weights))
	 (pad (/ (1- lw) 2))
	 (bincounts (send kdeobj :bin-counts))
	 kdelines
	 )
    (unless (> 20 lw 3)
	    (format t "don't like binning, num. weights: ~a~%" lw))
    (unless (= nbins (length bincounts))
	    (send kdeobj :bin-counts nil)
	    (setf bincounts (send kdeobj :bin-counts)))
    (setf kdelines 
	  (/ (vector-convolve (coerce weights 'vector)
			      (coerce bincounts 'vector) pad nbins)
	     (length (send kdeobj :data))))
  (if want-pairs
      (list xvals (coerce kdelines 'list))
    (coerce kdelines 'list))))

(defun calc-kd-direct-var-bw (data width type xvals method want-pairs kdeobj)
  (let ((dens
	 (mapcar #'(lambda (w x)
		     (first (calc-kernel-density-direct
			     data w type (list x x x) nil nil kdeobj)))
		 (exp (send (send kdeobj :slot-value 'bw-transf)
		       :transform xvals :want-pairs nil))
		 xvals)))
    (if want-pairs
	(list xvals dens)
      dens)))

(defun calc-kernel-density-direct (data width type xvals method want-pairs kdeobj)
  (let* ((width (if (send kdeobj :use-canonical)
		    (* width
		       (nth 5 (assoc type *kernel-data*)))
		  width))
	 #+udinaPatch (kdelines (kernel-dens data
					     :width (cond 
						     ((eql type 'u)
						      (* 1.5 width))
						     (t
						      width)) 
					     :type type
					     :xvals xvals))
;for mac, we have no extra kernels available
	 #-udinaPatch (kdelines (kernel-dens data
					     :width (cond
						     ((eql type 'h) 
						      (* 4 width ))
						     ((eql type 'u)
						      (* 1.5 width))
						     (t
						      width))
					     :type (if (eql type 'h) 'g type)
					     :xvals xvals))
	 )
; luke tierney uses a very special uniform kernel! must multiply by 9/8 (??)
    (when (eql type 'u) (setf (second kdelines)	;
			      (* (second kdelines) 1.125)))
; for modularity, this is the only call to kernel-dens, a xlispstat function
    (if want-pairs
	kdelines
      (second kdelines))))

(defun calc-kernel-density-update (data width type xvals method want-pairs kdeobj)
; see Fan-Marron(94) for full details
  (let* ((nbins (length xvals))
	 (delta (- (second xvals) (first xvals)))
	 (bincounts (send kdeobj :bin-counts))
	 (alfa (case (send kdeobj :kernel-type)
		     ('u 0)
		     ('a 1)
		     ('b 2)
		     ('w 3)))
	 (2alfa (* 2 alfa))
	 acoefs bcoefs alfaseq exs
	 from to bclist
	 (make-acoefs  (make-acoefs-function (send kdeobj :kernel-type)))
	 kdelines)
    (setq width (if (send kdeobj :use-canonical)
		    (* width
		       (nth 5 (assoc type *kernel-data*)))
		  width))
    (unless (= nbins (length bincounts))
	    (send kdeobj :bin-counts nil)
	    (setf bincounts (send kdeobj :bin-counts)))
; initially, from and to point to the beginning of the same list
; this list contains the xvals and the term to updating each xvals
    (setq bclist 
	  (mapcar #'(lambda (x c) 
		      (if (= c 0)
			  (list x 0)
			(list x (* c (pots (/ (- x) width) 2alfa)))))
		  xvals (coerce bincounts 'list)))
    (setq from bclist)
    (setq to bclist)
    (setq bcoefs (make-list (1+ (* 2 alfa)) :initial-element 0))
; this is the main loop, for each grid value within xvals,
; we must compute the estimate and add it to kdelines
    (dolist (grid xvals)
	    (setq acoefs (/ grid width))
	    (setq acoefs (funcall make-acoefs acoefs (- 1 (sqr acoefs))))
; while 'to <= g+h', add (pots -to/h) to bcoefs 
; and move 'to' to the right
	    (loop
	     (when (or (null to) (> (caar to) (+ grid width) ))
		   (return))
	     (setq bcoefs
		   (+ bcoefs (cadar to)))
	     (setq to (cdr to)))
; while 'from <= g-h', subtract pots from bcoefs
; and move 'from' to right
	    (loop
	     (when (or (null from) (> (caar from) (- grid width)))
		   (return))
	     (setq bcoefs
		   (- bcoefs (cadar from)))
	     (setq from (cdr from)))
;mult acoefs per bcoefs i sumar
	    (setq acoefs (/ (sum (* acoefs bcoefs)) width))
;dividir per n, data length
;afegir el resultat al davant de kdelines
	    (setq kdelines (cons acoefs kdelines)))
    (setq kdelines (reverse (/ kdelines (length data))))
    (if want-pairs
	(list xvals (coerce kdelines 'list))
      (coerce kdelines 'list))
    ))

(defun calc-kd-update-var-bw-x (data width type xvals method want-pairs kdeobj)
; see Fan-Marron(94) for full details
  (let* ((nbins (length xvals))
	 (datasize (car (last (send kdeobj :fivnums))))
	 (delta (- (second xvals) (first xvals)))
	 (alfa (case (send kdeobj :kernel-type)
		     ('u 0)
		     ('a 1)
		     ('b 2)
		     ('w 3)))
	 (2alfa (* 2 alfa))
	 (acoefs 23)
	 (vary-on (send kdeobj :variable-bandwidth))
	 (from -1)
	 (to 0)
	 nfrom nto
	 (make-acoefs  (make-acoefs-function (send kdeobj :kernel-type)))
	 kdelines
	 (xvals (coerce xvals 'vector))
	 (width (coerce width 'vector))
	 (width (if (send kdeobj :use-canonical)
		    (* width
		       (nth 5 (assoc type *kernel-data*)))
		  width))
	 (bincounts (send kdeobj :bin-counts))
	 (bincounts (if (= nbins (length bincounts))
			(coerce bincounts 'vector)
		      (progn
			(send kdeobj :bin-counts nil)
			(coerce (send kdeobj :bin-counts) 'vector))))
	 (bsum (make-list (1+ 2alfa) :initial-element 0));the cumulative sums
	 (bterms  ;the terms to cumulate
	  (if (eq 'on-data-values vary-on)
	      (coerce 
	       (map-elements #'(lambda (c x h)
				 (* c 
				    (expt h (- (1+ 2alfa)))
				    (pots (- x) 2alfa)))
			     bincounts
			     xvals
			     width)
	       'vector)
	    (coerce ; we have bandw vary 'on-x-values
	     (map-elements #'(lambda (c x)
			       (* c
				  (pots (- x) 2alfa)))
			   bincounts
			   xvals)
	     'vector))))
    (flet ; some local function
     ((calc-from-to (i);sets nfrom and nto
		    (setf nto (ceiling (/ (aref width i) delta)))
		    (setf nfrom (- i nto))
		    (when (< nfrom -1)
			  (setf nfrom -1))
		    (setf nto (+ i nto))
		    (when (> nto  nbins)
			  (setf nto nbins))))
     (dotimes (i  nbins)
	      (calc-from-to i)
	      (while (< nfrom from)
		(setf bsum (+ bsum (aref bterms from)))
		(decrement from))
	      (while (> nfrom from)
		(setf bsum (- bsum (aref bterms (1+ from))))
		(increment from))
	      (while (> nto to)
		(setf bsum (+ bsum (aref bterms to)))
		(increment to))
	      (while (< nto to)
		(setf bsum (- bsum (aref bterms (1- to))))
		(decrement to))
	      
	      (if (eq vary-on 'on-data-values)
		  (setq acoefs (aref xvals i))
		(setq acoefs (/ (aref xvals i) (aref width i))))
	      (setq acoefs (funcall make-acoefs acoefs (- 1 (sqr acoefs))))
;mult acoefs per bcoefs i sumar
	      (if (eq vary-on 'on-data-values)
		  (setq acoefs (sum (* acoefs bsum)))
		(setq acoefs (/ (sum (* acoefs 
					bsum
					(/ (pots (aref width i) 2alfa))))
				(aref width i))))
 ;afegir el resultat al davant de kdelines
	      (setq kdelines (cons acoefs kdelines)))
 ;dividir per n, data length
    (setq kdelines (reverse (/ kdelines datasize)))
    (if want-pairs
	(list xvals (coerce kdelines 'list))
      (coerce kdelines 'list))
    )))



(defun calc-kd-update-var-bw-data (data width type xvals method want-pairs kdeobj)
; see Fan-Marron(93) for full details
  (let* ((numbins (length xvals))
	 (datasize (car (last (send kdeobj :fivnums))))
	 (alfa (position type '(u a b w)))
	 (2alfa (* 2 alfa))
	 acoefs
	 (make-acoefs  (case type
			     ('u #'(lambda (x) '(1)))
			     ('a #'(lambda (x) (pots x 2)))
			     ('b #'(lambda (x) (pots x 4)))
			     ('w #'(lambda (x) (pots x 6)))))
	 (calpha  (case type
			     ('u 0.5)
			     ('a 0.75)
			     ('b 0.9375)
			     ('w 1.09375)))
	 kdelines
	 (xvals (coerce xvals 'vector))
	 (delta (- (aref xvals 1) (aref xvals 0)))
	 (width (if (send kdeobj :use-canonical)
		    (* (coerce width 'vector)
		       (nth 5 (assoc type *kernel-data*)))
		  (coerce width 'vector)))
	 (bincounts (send kdeobj :bin-counts))
	 (bincounts (if (= numbins (length bincounts))
			(coerce bincounts 'vector)
		      (progn
			(send kdeobj :bin-counts nil)
			(coerce (send kdeobj :bin-counts) 'vector))))
	 (bsum (make-list (1+ 2alfa)
			  :initial-element 0));the cumulative sums to be
	 (bterms  (make-array numbins :initial-element bsum )); the terms to cumulate
	 xj
	 ind
	 (warn nil)
	 (calc-bterm (case alfa
			   (0 #'(lambda (i)
				  (list (/ (aref bincounts i) (aref width i)))))
			   (1 #'(lambda (i)
				  (let* ((xi (aref xvals i))
					 (hi (aref width i))
					 (qi (/ xi hi)))
				    (* (/ (aref bincounts i) hi)
				       (list (- 1 (* qi qi)) 
					     (/ (* 2 qi) hi) 
					     (- (/ (* hi hi))))))))
			   (2 #'(lambda (i)
				  (let* ((xi (aref xvals i))
					 (hi (aref width i))
					 (ihi (/ hi))
					 (ihi2 (* ihi ihi))
					 (qi (/ xi hi))
					 (qi2 (* qi qi)))
				    (* (/ (aref bincounts i) hi)
				       (list (+ 1 (* -2 qi2) (* qi2 qi2))
					     (* 4 ihi (- qi (* qi qi2)))
					     (* ihi2 (+ -2 (* 6 qi2)))
					     (* -4 qi (* ihi ihi2))
					     (* ihi2 ihi2))))))
			   (3 #'(lambda (i)
				  (let* ((xi (aref xvals i))
					 (hi (aref width i))
					 (hi2 (* hi hi))
					 (ihi (/ hi))
					 (ihi2 (* ihi ihi))
					 (ihi4 (* ihi2 ihi2))
					 (qi (/ xi hi))
					 (qi2 (* qi qi))
					 (qi/hi (/ qi hi))
					 (qi/hi2 (* qi/hi qi/hi)))
				    (* (/ (aref bincounts i) hi)
				       (list (+ 1 (* -3 qi2) (* 3 qi2 qi2) 
						(* -1 qi2 qi2 qi2))
					     (+ (* 6 qi/hi) (* -12 qi/hi qi2)
						(* 6 qi/hi qi2 qi2))
					     (+ (* -3 ihi2) (* 18 qi/hi2)
						(* -15 qi/hi2 qi2))
					     (+ (* -12 qi/hi ihi2)
						(* 20 qi/hi qi/hi2))
					     (+ (* 3 ihi2 ihi2)
						(* -15 qi/hi2 ihi2))
					     (* 6 qi/hi ihi4)
					     (* -1 ihi2 ihi4)))))))))
;first pass, annotate the index for updating
    (dotimes (i numbins)
	     (unless (= 0 (aref bincounts i))
		     (let* ((term (funcall calc-bterm i))
			    (d (/ (aref width i) delta))
			    (inf (max 0 (- i (floor d))))
			    (sup (min (1- numbins) (+ i (ceiling d)))))
		       (when (and  *kde*computing*inspect* (< d 4))
			     (setf warn t)
			     (print 
			      (format nil
				      "~a~a x=~a h=~a"
				      "warning: bandwidth too small "
				      "or grid width too big"
				      (aref xvals i)
				      (aref width i))))
		       (setf (aref bterms inf)
			     (+ (aref bterms inf) term))
		       (setf (aref bterms sup)
			     (- (aref bterms sup) term)))))
    (dotimes (j  numbins)
	     (setf xj (aref xvals j))
	     (setf bsum (+ bsum (aref bterms j)))
	     (setq acoefs (funcall make-acoefs xj))
 ;mult acoefs per bcoefs i sumar
	     (setq acoefs (sum (* calpha acoefs bsum)))
 ;afegir el resultat al davant de kdelines
	     (setq kdelines (cons acoefs kdelines)))
 ;dividir per n, data length
    (setq kdelines (reverse (/ kdelines datasize)))
    (when warn
	  (message-dialog
	    (format nil
		    "~a~%~a~%~a"
		    "WARNING: bandwidth too small"
		    "or grid width too big."
		    "DON'T trust the estimates you see.")))
    (if want-pairs
	(list xvals (coerce kdelines 'list))
      (coerce kdelines 'list))
    ))

(defun calc-kernel-density-fast-fourier-transform (bincounts width type xvals 
							     want-pairs kdeobj
							     fft-counts)
  (let* ((nbins (length xvals))
	 (xmin (first xvals))
	 (delta (- (second xvals) xmin))
	 (xmax (+ xmin (* delta (1+ nbins))))
	 (weights (send kdeobj :kernel-weights type delta width))
	 (lw (length weights))
	 (pad (/ (1- lw) 2))
	 (bincounts (if bincounts bincounts
		      (send kdeobj :bin-counts)))
         (fft-counts (if fft-counts fft-counts
		       (send kdeobj :fft-counts)))
	 kdelines)
    (unless (= nbins (length bincounts))
	    (format *STANDARD-OUTPUT* ;;;temp
		    "in ftt nbins was diff than bincounts: ~a ~a ~%"
		    nbins bincounts)
	    (send kdeobj :bin-counts nil)
            (setf bincounts (send kdeobj :bin-counts))
	    (setf fft-counts (send kdeobj :fft-counts)))
    (setf kdelines 
	  (/ (fft-convolve (coerce weights 'vector)
			   (coerce bincounts 'vector) pad nbins fft-counts)
	     (length (send kdeobj :data))))
    (if want-pairs
	(list xvals (coerce kdelines 'list))
      (coerce kdelines 'list))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;convolution functions
;;;
(defun vector-convolve (weights counts 
				&optional (pad (/ 2 (1- (length weights)))) 
				(ncounts (length counts)))
"pad is semilength of weights, ncounts is length of counts"
(let ((2pad (* 2 pad))
      (lw (length weights))
      (result (make-array ncounts)))
  (dotimes (i (- ncounts (* 2 pad)))
	   (setf (aref result (+ pad i))
		 (sum (* weights 
			 (select counts (iseq i (+ i 2pad)))))))
;after computing the central part of the vector, must compute
;the low and high ends that use only part of the weights
  (dotimes (i pad)
	   (setf (aref result i)
		 (sum (* (select weights 
				 (iseq (- pad i) (1- lw)))
			 (select counts (iseq (+ 1 i pad))))))
	   (setf (aref result (- ncounts i 1)) 
		 (sum (* (select weights 
				 (iseq (+ lw i (- pad))))
			 (select counts (iseq (- ncounts i 1 pad )
					      (- ncounts 1)))))))
  result))



;this is the call to fft-convolve
;(setf kdelines 
;      (/ (fft-convolve (coerce weights 'vector)
;		        (coerce bincounts 'vector)
;                       pad nbins fft-counts)
;         (length (send self :data)))))
;

;;;comment out
#|
(defun fft-convolve (weights counts
			     &optional (semi (/ (1- (length weights)) 2)) 
			     (ncounts (length counts))
			     (fft-counts nil))
  (let* ((weights (coerce weights 'vector))
	 (counts (coerce counts 'vector))
	 (p 1)
	 ;let p be the min power of 2 greater than semi+ncounts
	 (p (loop (if (> p (+ semi ncounts)) (return p) (setq p (* 2 p)))))
	 (arw (make-array p :initial-element 0))
         arc
         arwf arcf hadprod result)
    ;pad weights with zeroes in arw
    (setf (aref arw 0)
             (aref weights semi))
    (dotimes (i semi)
       (setf (aref arw (1+ i))
             (aref weights (+ semi i 1)))
       (setf (aref arw (- p i 1))
                  (aref weights (- semi i 1))))
    ;pad counts with zeroes in arc
    ;if needed, build arc for fft'ing it in arcf
    (if (and fft-counts (= p (length fft-counts)))
        (setf arcf fft-counts)
        (progn
	  (when *kde*computing*inspect*
		(format t "padding the fft vector, ~a to ~a~%"
			       (length fft-counts) p))
	  (setf arc (make-array p :initial-element 0))
          (dotimes (i ncounts)
             (setf (aref arc i)
                   (aref counts i)))
          (setf arcf (fft arc))))
    ;fft direct for arw 
    (setf arwf (fft arw))
(setq fc arcf)
;;(format t "weights: ~a~%arw: ~a~%" weights arw)
;hadamard multiply
    (setf hadprod (* arwf arcf))
;fft inverse and keep only the relevant part
    (setf result (select (/ (fft hadprod t) p) (iseq ncounts)))
;trim imaginary parts
    (setf result (map-elements #'realpart result))
;and put near zero to zero
    (setf result (map-elements #'(lambda (x) (if (< (abs x) 1e-14) 0 x))
                         result))
result
))

|#
;;;a new version, better?
;this is the call to fft-convolve
;(setf kdelines 
;      (/ (fft-convolve (coerce weights 'vector)
;		        (coerce bincounts 'vector)
;                       pad nbins fft-counts)
;         (length (send self :data)))))
;



(defun fft-convolve (weights counts
			     &optional semi 
			     ncounts
			     fft-counts)
"Args: weights counts &optional semi ncounts fft-counts
computes the convolution of vectors WEIGTHS and COUNTS.
 Optionally, SEMI is the number of zeros to pad the weigths vector,
ncounts is the length of counts, and fft-counts is the pre-computed fft 
of the counts vector"
  (let* ((semi (if semi semi
		 (/ (1- (length weights)) 2)))
	 (weights (coerce weights 'vector))
	 (counts (coerce counts 'vector))
	 (ncounts (if ncounts ncounts
		      (length counts)))
	 (p 1)
	 ;let p be the min power of 2 greater than semi+ncounts
	 (p (loop (if (> p (+ semi ncounts)) (return p) (setq p (* 2 p)))))
	 arw 
         arc
         arwf arcf hadprod result)
    ;pad counts with zeroes in arc
    ;if needed, build arc for fft'ing it in arcf
    (when fft-counts
	  (cond
	   ((= p (length fft-counts))
	    (setf arcf fft-counts))
	   ((< p (length fft-counts))
	    (setq p (length fft-counts))
	    (setf arcf fft-counts))
	   ((> p (length fft-counts))
	    (setf fft-counts nil))))
    (unless fft-counts
	    (when *kde*computing*inspect*
		  (format t "padding the fft vector, ~a to ~a~%"
				 (length fft-counts) p))
	    (setf arc (make-array p :initial-element 0))
	    (dotimes (i ncounts)
		     (setf (aref arc i)
			   (aref counts i)))
	    (setf arcf (fft arc)))
    ;pad weights with zeroes in arw
    (setq arw (make-array p :initial-element 0))
    (setf (aref arw 0)
             (aref weights semi))
    (dotimes (i semi)
       (setf (aref arw (1+ i))
             (aref weights (+ semi i 1)))
       (setf (aref arw (- p i 1))
                  (aref weights (- semi i 1))))
    ;fft direct for arw 
    (setf arwf (fft arw))
;;a global, for inspection
(setq fc arcf)
;;(format t "weights: ~a~%arw: ~a~%" weights arw)
;hadamard multiply
    (setf hadprod (* arwf arcf))
;fft inverse and keep only the relevant part
    (setf result (select (/ (fft hadprod t) p) (iseq ncounts)))
;trim imaginary parts
;    (setf result (map-elements #'realpart result))
;and put near zero to zero
    (setf result (map-elements
		  #'(lambda (x)
		      (setf x (realpart x))
		      (if (< (abs x) 1e-14) 0 x))
                         result))
result
))




;;for updating method


(defun make-acoefs-function (type)
"returns a funcallable function for the given kernel type of the beta familiy.
 For speeding things, the function takes an extra argument dependent on the first.
 Details in Fan-Marron 94"
  (case type
	('u #'(lambda (a 1-a2) (list 0.5)))
	('a #'(lambda (a 1-a2) 
		(* 0.75 (list 1-a2 (* -2 a) -1))))
	('b #'(lambda (a 1-a2)
		(* 0.9375 
		   (list (sqr 1-a2)
			 (* -4 a 1-a2)
			 (* -2 (- (* 3 1-a2) 2))
			 (* 4 a)
			 1))))
	('w #'(lambda (a 1-a2)
		(* 1.09375
		   (list (* 1-a2 1-a2 1-a2)
			 (* -6 a (sqr 1-a2))
			 (* -3 1-a2 (- (* 5 1-a2) 4))
			 (* 4 a (- (* 5 1-a2) 2))
			 (* 3 (- (* 5 1-a2) 4))		
			 (* -6 a)
			 -1))))))


(defun pots (x n)
"returns a list of powers 0..n of x"
(mapcar #'(lambda (i) (expt x i))
	(iseq (1+ n))))

(defun pots2 (x n)
"like pots, it turns to be slower, factor 2"
(accumulate #'(lambda (a b) (* a b x))
	    (make-list (1+ n) :initial-element 1)))
;accumulate is implemented in xlisp, in ???

;alternative to vector-convolve, 
;fill counts with zeros below and above
;and do only one loop:
; MUCH MORE SLOWER!!!

(defun vector-fill-convolve (weights counts 
				&optional (pad (/ 2 (1- (length weights)))) 
				(ncounts (length counts)))
"pad is semilength of weights, ncounts is length of counts"
(let* ((2pad (* 2 pad))
       (lw (length weights))
       (result (make-array (+ ncounts 2pad) :initial-element 0)))
  (setf (select result (iseq pad (+ ncounts pad -1)))
        counts)
  (dotimes (i ncounts)
           (setf (aref result (+ i pad))
                 (sum (* weights)
                      (select result (iseq i lw)))))
  (select result (iseq pad (+ ncounts pad -1)))))

;encore an alternative to vector-convolve, 
;similar, fill counts with zeros below and above
;and do only one loop:
; a little SLOWER than the first
(defun vector-fill-convolve (weights counts 
				&optional (pad (/ 2 (1- (length weights)))) 
				(ncounts (length counts)))
"pad is semilength of weights, ncounts is length of counts"
(let* ((2pad (* 2 pad))
       (lw (1- (length weights)))
       (fullcounts (make-array
                (+ ncounts 2pad)
                :initial-contents (append (repeat 0 pad)
                                          (coerce counts 'list)
                                          (repeat 0 pad))))
       (result (make-array ncounts :initial-element 0)) )
  (dotimes (i ncounts)
           (setf (aref result i)
                 (sum (* weights
                         (select fullcounts (iseq i (+ i lw)))))))
result))

;a last trial with lists
(defun vector-fill-convolve (weights counts 
				&optional (pad (/ 2 (1- (length weights)))) 
				(ncounts (length counts)))
"pad is semilength of weights, ncounts is length of counts"
(let* ((lw (1- (length weights)))
       (weights (coerce weights 'list))
       (counts (coerce counts 'list))
       (2pad (* 2 pad))
       (fullcounts (append (repeat 0 pad) counts (repeat 0 pad))))
  (maplist
   #'(lambda (x) (sum (* weights
                         (select x (iseq 0 lw)))))NOVANOVA!!
   fullcounts)))


(defun test ()
  (vector-convolve #(.1 .3 .5 .3 .1) #(1 2 3 4 5 6 7) 2 7))
(defun filltest ()
  (vector-fill-convolve #(.1 .3 .5 .3 .1) #(1 2 3 4 5 6 7) 2 7))
      

(defun test2 ()
  (vector-convolve #(.1 .3 .5 .3 .1 .1 .3 .5 .3 .1 .2)
		   bc5
		   5 400))


(defun ftest2 ()
  (vector-fill-convolve #(.1 .3 .5 .3 .1 .1 .3 .5 .3 .1 .2)
		   bc5
		   5 400))
		   
		   
                   ;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some utility functions

(defun quartiles (x) 
  (quantile x '(0 .25 .5 .75 1)))

(defun logseq (from to num)
"logarithmic version of rseq"
      (exp (rseq (log from) (log to) num)))

(defun iqr (data)
  (apply #'(lambda (q0 q1 q2 q3 q4) (- q3 q1))
	 (fivnum data)))

(defun count-values (data &key (sorted nil))
"Args: data &key sorted
 dirty version of a counting function. Returns
 a list of pairs (value frequency)"
(let* ((data (if sorted (copy-list data) (sort (copy-list data) #'<=)))
       (bc (bin-count-sorted-data data data))
       (par (transpose (list data bc))))
  (delete-if-not #'(lambda (x) (not (eql 0 (second x)))) par)
  par))

(defun sqr (x) (* x x))


(defun format-digits (num &optional (number-of-digits 3))
"returns NUM with at most NUMBER-OF-DIGITS significant digits. Vectorized."
  (if (consp num)
      (mapcar #'(lambda (x) (format-digits x number-of-digits))
              num)
      (if (= 0 num)
	  0
	  (let* ((sign (if (> num 0)
                       1
                       (progn (setq num (- num))
                              -1)))
             (order (floor (/ (log num) (log 10))))
             (inf (- order number-of-digits -1))
             (tr (round (/ num (expt 10 inf))))
             (res (* sign tr (expt 10 inf))))
        res))))



(defun find-local-minima (line)
"given a list of points (xi yi)
 returns a list of the points that are local minima"
  (unless line (return nil))
  (let ((amin nil)
	(mins nil))
    (dotimes (i (- (length line) 2) mins)
      (setq amin 
	    (localminp (nth i line) (nth (+ 1 i) line) (nth (+ 2 i) line)))
      (when amin (setq mins (cons amin mins))))))

(defun localminp (p1 p2 p3)
  (if (and (>= (second p1) (second p2))
	   (<= (second p2) (second p3)))
      p2))

(defun find-minimum (pair-list)
"given a list of points (xi yi)
 returns the point with minimum yi"
  (when pair-list
   (let ((min (first pair-list)))
      (dolist (p pair-list)
         (when (< (second p) (second min))
            (setf min p)))
      min)))


(unless (fboundp 'strcat)
      (defun strcat (&rest args)
	(apply #'concatenate 'string args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; automatic bandwidth selection methods
;;; many of them rely on running a patched version of xlisp
;;; including a file banbodies.c


;;;;;;;
; least-squares cross-validation method for bandwith-selection

#+udinaPatch
(defun lscv (data band)
  (bandwidth-crit-lscv data band))

#-udinaPatch
(defun lscv (data band)
  "Computes the cross-validatory function for least-squares cross-validation"
  (let* ((di data)
	 (dj (cdr di))
	 (cv 0)
	 (n (length data))
	 (xt 0))
    (dotimes (i (1- n))
      (setq dj (cdr di))
      (dotimes (j (- n i 1))
	
	(setq xt (exp (/ (sqr (/ (- (car di) (car dj)) 
				 band))
			 -2)))
	(setq cv (+ cv (/ (sqrt xt) 2 (sqrt pi))))
	(setq cv (- cv (* sqrt2bypi xt)))
	
	(setq dj (cdr dj)))
      (setq di (cdr di)))
    
    (setq cv (/ (+ (/ cv (1- n) .5) 
		   1by2sqrtpi ) 
		(* n band)))))

(setq sqrt2bypi (sqrt (/ 2 pi)))
(setq 1by2sqrtpi (/ 1 (* 2 (sqrt pi))))

;;;;;;;;;
;sheater-jones algorithm for bandwidth selection
;needs finding the root of sj(h)=0
#+udinaPatch
(defun sj-plug-in (data band)
  (+ band
     (bandwidth-crit-sj  data (float band) (float (iqr data)))))

#-udinaPatch
(defun sj-plug-in (data band)
  (error "sj-plug-in function not available" nil))

;;/* ----- SJ Algorithm ----- */
(defun binned-sj-plug-in (xmin delta numx bincounts iqr bandwidth datalen)
"given binned data, gives the Sheather-Jones score for the bandwidth given.
 Ths bandwidth to select must give 0 score"
;;
;;
;;double band_sel_sj(doubleptr,width,lambda,n)
;;double         doubleptr[],width,lambda;
;;int n;
;;{
(let* ((a (* 0.920 iqr (expt datalen -0.142857142857)))
       (inva (/ a))
       (b (* 0.912 iqr (expt datalen -0.111111111)))
       (invb (/ b))
       (sj1 0)
       (sj2 0)
       aux g tel noe 
       (sji 0)
       gi xt )
;these are the derivative gaussian evaluation functions, to be
;improved for this binned case
  (flet ((l4 (x2)
	     (* (exp (* -0.5 x2)) (+ 3 (* x2 (- x2 6)))))
	 (l6 (x2)
	     (* (exp (* -0.5 x2))
		(+ -15.0 (* x2 (+ 45.0 (* x2 (- x2 15.0)))))))) 
;;     for (j = 0; j < (n-1); j++) {
;;        for (k = j + 1; k < n; k++) {
;;            aux = (doubleptr[j] - doubleptr[k]);
;;            aux = aux*aux;
;;            xt = aux / sqra;
;;            sj1 = sj1 + (exp(-0.5*xt))*((xt - 6.0)*xt + 3.0);
;;            xt = aux / sqrb;
;;            sj2 = sj2 + (exp(-0.5*xt))*(((xt - 15.0)*xt + 45.0)* xt - 15.0);
;;	}
;;    }
	(dotimes (j numx)
		 (when (> (aref bincounts j) 0)
		       (dotimes (k j)
				(setq xt (* delta (- j k) inva))
				(setq xt (* xt xt))
				(setq sj1 (+ sj1 (* (l4 xt)
						    (aref bincounts j)
						    (aref bincounts k))))
				(setq xt (* delta (- j k) invb))
				(setq xt (* xt xt))
				(setq sj2 (+ sj2 (* (l6 xt)
						    (aref bincounts j)
						    (aref bincounts k)))))))
;;    tel = -6.0*sqrt(2.0)*(2.0*sj1 + 3.0*n);
	(setq tel (* -6.0 (sqrt 2) (+ (* 2 sj1) (* 3 datalen))))
;;    noe = pow(a,5) * (2.0*sj2 - 15*n);
	(setq noe (* (expt a 5) (- (* 2 sj2) (* 15 datalen))))
;;    g = b * pow(tel/noe,1.0/7.0);
	(setq g (* b (expt (/ tel noe) 0.142857142857)))
	(when (< (/ tel noe) 0)
	      (print "negative in expt 1/7"))
;;    gi = g * pow(width,5.0/7.0);
	(setq gi (* g (expt bandwidth 0.7142857142857)))
;;    sji = 0.0;
;;    for (j = 0; j < (n-1); j++) {
;;       for (k = j + 1; k < n; k++) {
;;           aux = (doubleptr[j] - doubleptr[k])/gi;
;;           xt = aux*aux;
;;           sji = sji + (exp(-0.5*xt))*((xt - 6.0)*xt + 3.0);
;;       }
;;    }
	(dotimes (j numx)
		 (when (> (aref bincounts j) 0)
		       (dotimes (k j)
				(setq xt (/ (* delta (- j k)) gi))
				(setq xt (* xt xt))
				(setq sji (+ sji (* (l4 xt)
						    (aref bincounts j)
						    (aref bincounts k)))))))
;;    tel = n - 1;
;;    noe = sqrt(2.0) * (2.0*sji + 3*n);
	(setq noe (* (sqrt 2) (+ (* 2 sji) (* 3 datalen))))
;;    sji = gi * pow(tel/noe,0.2) - width;
	(when (< (/ (1- datalen) noe) 0)
	      (print "negative in expt 1/5"))
	(- (* gi (expt (/ (1- datalen) noe) 0.2)) bandwidth)))
;;    return (sji);
;;}
)

;;;;;;;this is the function we use in kde
;;;;;;;
(defun sheater-jones-bandwidth (x &optional (lambda (iqr x)))
  "This subroutine calculates the Sheather-Jones
'solve-the-equation' bandwidth for a Gaussian kernel estimate.
(Journal of the Royal Statistical Society, Series B,
1991, Vol. 53, pp 683-690). 
Code is translation from Fortran code by Simon Sheather.
This is the original documentation:
Double precision is used throughout.
IMPORTANT INPUT VARIABLES
n = sample size
x = vector containing the data
lambda = interquartile range (iqr)
IMPORTANT OUTPUT VARIABLES
hsjd = value of Sheather-Jones bandwidth
OTHER IMPORTANT VARIABLES
hnml = bandwidth based on iqr assuming data is normal
       (used as a starting value in this subroutine)
chat = estimate of constant c in bandwidth a (i.e. a = c. h**(5/7))"
  (let* (hsjd
	 l2 k1 k2
	 (tola 1.0d-5) (tolg 1.0d-4) (loop 15)
	 (n (length x))
	 (lam lambda)
	 (k3  (/ (- n 1.0) (sqrt 2)))
	 (threen  (* n 3.0))
	 (rt2pi (sqrt (* 2 pi)))
	 (hnml (/ (* 0.79 lambda) (expt n 0.2)))
	 (l2 (* lambda lambda))
       ;; ESTIMATE VALUE OF c
	 (ac  (/ (* 0.92 lambda) (expt n (/ 7.0))))
	 (ac5 (expt ac 5.0))
	 (bc  (/ (* 0.912 lambda) (expt n (/ 9.0))))
	 (sa  0) (sb 0)
	 (x (coerce (sort-data x) 'vector))
	 xij da db ea eb rhat2 rhat3 chat chat7 
	 firsth ainit y y2 e a s tt anew gprime nloop)
    (macrolet
     ((signe (num)
	     `(if (> ,num 0) 1 (if (< ,num 0) -1 0)))
      (fortran-dsigne (a b)
		      `(if (< ,b 0)
			   (- (abs ,a))
			 (abs ,a)))
      (square (num)
	      `(* ,num ,num)))
     (print (select x (list 0 (1- n))))
     (dotimes (i (1- n))
	      (dotimes (j (- n i 1))
		       (setq xij (- (aref x i) (aref x (+ i j 1))))
		       (setq da (square (/ xij ac))) 
		       (setq db (square (/ xij bc)))
		       (setq ea (exp (* -0.5 da)))
		       (setq eb (exp (* -0.5 db)))
		       (setq sa (+ sa (* ea (+ (square da)
					       (* -6.0 da) 3.0))))
		       (setq sb (+ sb (* eb (+ (* db db db) 
					       (* -15.0 (square db))
					       (* 45.0 db) -15.0))))))
     (setq rhat2 (/ (* 2.0 sa)  (* n (1- n) ac5 rt2pi)))
     (setq rhat2 (+ rhat2 (/ 3.0 (* rt2pi (1- n) ac5))))
     (setq rhat3 (/ (* -2.0 sb) (* n (1- n) (expt bc 7) rt2pi)))
     (setq rhat3 (+ rhat3 (/ 15.0 (* rt2pi (1- n) (expt bc 7)))))
     (setq chat (* 1.357 (expt (abs (/ rhat2 rhat3)) (/ 7))))
     (setq chat7 (expt chat 7.0))
	    ;;	*** USE NEWTON-RAPHSON METHOD TO CALCULATE h ***
	    ;;	INITIAL VALUES FOR h AND a
     (setq firsth  hnml) 
     (setq ainit (* chat (expt firsth (/ 5.0 7.0))))
     (catch 'end-of-loop 
       (loop
	(setq a (* 1.5 ainit))
	(dotimes (m loop)
		 (setq s 0.0)
		 (setq tt 0.0)
		 (dotimes (i (1- n))
			  (dotimes (j (- n i 1))
				   (setq xij (- (aref x i)
						(aref x (+ i j 1))))
				   (setq y (/ xij a))
				   (setq y2 (square y))
				   (setq e (exp (* -0.50 y2)))
				   (setq s (+ s (* e (+ (square y2)
							(* -6.0 y2) 3.0))))
				   (setq tt (+ tt (* e y2 (+ (square y2) 
							     (* -10.0 y2) 
							     15.0))))))
		 (setq tt (* 2.0 tt))
		 (setq s (+ (* 2.0  s)  threen))
		 (setq k1 (/ (square a) chat7))
		 (setq k1 (fortran-dsigne (expt (abs k1) 0.2d0) k1))
		 (setq k2 (/ k3 s))
		 (setq k2 (fortran-dsigne (expt (abs k2) 0.2d0) k2))
		 (setq gprime  (* -0.2d0 (+ (* k2 (- (/ tt s) 5.0d0))
					    (* 7.0d0 k1))))
		 (setq g (* a (- k2 k1)))
		 (setq anew (-  a (/ g gprime)))
			;;		CALCULATE h FROM a
		 (setq k1 (* anew (/ anew chat7)))
		 (setq k1 (fortran-dsigne (expt (abs k1) 0.2d0) k1))
		 (setq h (* k1 anew))
		 (if (< (abs (- anew a)) tola)
		     (throw 'end-of-loop nil))
		 (if (< (abs g) tolg)
	             (throw 'end-of-loop nil))
		 (if (<= anew 0.0d0)
		     (progn (format t "~s~s" 
				    "NEWTON'S METHOD UNABLE TO"
				    " FIND SOLUTION TO sjeqd~%")
			    (setq ainit (* 1.5d0 ainit)))
		   (setq a anew)))))
     h)))





;;;;;;;;;
;park-marron algorithm for bandwidth selection
;needs finding the root of pm(h)=0
(defun pm-plug-in (data band); data are assumed to be incr. sorted
"plug-in method by park and marron, 1990
 This function actually gives pm(h)+h"
  (let* ((di (coerce data 'vector))
	 (n (length data))
	 (iqr  (iqr di))
	 (pm 0)
	 (xt 0)
	 (pm-constant 0.886491083457929)
	 (g (* pm-constant (expt iqr (/ 3 13))))
	 (gi (* g (expt band (/ 10 13)))))
    (dotimes (i (1- n))
      (dotimes (j (- n i 1))
	(setq xt (/ (sqr (/ (- (aref di i) (aref di (+ i j 1)))
			    gi))
		    2))
	(setq pm (+ pm (* (exp (/ xt -2)) (+ 3 (* xt (- xt 6))))))))
    (setq pm (* gi (expt (/ pm 2 n) -0.2)))))
	


;;;;#-udinaPatch
(defun hsjm (data lambda) 
  (let* ((n (length data))
	 (a (* 4.29 lambda (expt n (/ -1 11))))
	 (sqra (* a a))
	 (b (* 0.91 lambda (expt n (/ -1 9))))
	 (sqrb (* b b))
	 (i2 0)
	 (i3 0)
	 aux h xt tel noe j1 j2 )
    (dotimes (j (- n 1))
	     (dotimes (k (- n j 1))
		      (setq aux (- (select data j) (select data (+ k j 1))))
		      (setq aux (* aux aux))
		      (setq xt (/ aux sqra))
		      (when (< xt 1)
			    (setq xt (+ 756 (* xt 
					       (+ -29700
						  (* xt (+ 200200
							   (* xt (+ -491400
								    (* xt (+  504900
									      (* xt -184756)))))))))))
			    (setq i2 (+ i2 (* xt (/ 135135 16384)))))
		      (setq xt (/ aux sqrb))
		      (setq i3 (+ i3 (* (exp (* -0.5 xt))
					(+ -15 (* xt
						  (+ 45 (* xt
							   (+ -15 xt))))))))))
    (setq i2 (/ (+ (* 2 i2)
		   (* n 756 (/ 135135.0 16384.0)))
		(* n (- n 1) (expt a 5))))
    (setq tel (- (* 2 i3) (* 15 n)))
    (setq noe (* (sqrt (* 2 pi)) n (- n 1) (expt b 7)))
    (setq i3 (/ (- tel) noe))
    (setq j2 (/ (* 3 i3) (* 20 i2)))
    (setq j1 (/ 1 (* 2 (sqrt pi) i2 n)))
    (+ (expt j1 0.2) (* j2 (expt j1 0.6)))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; data for some kernels, some of them calculated with Mathematica.

#+udinaPatch (setq *built-in-kernels*
		   '((b . "Bisquare")
		     (e . "Epanechnikov")
		     (a . "Bartlett-Epanechnikov")
		     (h . "Gaussian")
		     (g . "Narrow gaussian")
		     (u . "Uniform")
		     (t . "Triangular")
		     (r . "Order 4 kernel")
		     (w . "Triweight")))

#-udinaPatch (setq *built-in-kernels* 
		   '((b . "Bisquare")
		     (g . "Narrow gaussian")
		     (h . "Gaussian")
		     (u . "Uniform")
		     (t . "Triangular")))	       


(setq *kernel-names*
		   '((b . "Biweight")
		     (e . "Epanechnikov")
		     (a . "Bartlett-Epanechnikov")
		     (h . "Gaussian")
		     (g . "Narrow gaussian")
		     (u . "Uniform")
		     (t . "Triangular")
		     (r . "Order 4 kernel")
		     (w . "Triweight")
))


;;;for each kernel, his letter is assoc'd with a list of, in order,
; the letter code
; R(K), integral of squared kernel
; \mu_{2}, integral of t^2*k(t)
; integral of squared second derivative
; name
; canonical rescaling constant delta_0 (Marron-Dolan 88)
; function to compute kernel value
; relevant support of the unscaled function: only the high end
(defvar *kernel-data*
'((h  0.2820947917739668251
      1
      0.003305798341102127
      "Gaussian"
      .7764
      (function normal-dens)
      4)
  (b  0.7142857142857142857
      0.1428571428571428572
      22.5
      "Biweight"
      2.0362
      (function (lambda (x) (if (< (abs x) 1)
                                (* .9375 (sqr (1- (sqr x))))
                                0)))
      1)
  (a  0.2683281572999747635
      1.
      0.08049844718999242907
      "Bartlett-Epanechnikov"
      1.7188
      (function (lambda (x) (if (< (abs x) 1)
                                (* 0.75 (- 1  (sqr x)))
                                0)))
      1)
  (e  0.6
      0.2
      4.5
      "Epanechnikov"
      0.768658
      (function (lambda (x) (if (< (abs x) 2.23606797749979)
                                (* 0.335410196625 (- 1 (/ (sqr x) 5)))
                                0)))
      2.23606797749979)
  (t  0.6666666666666666667
      0.1666666666666666667
      nil
      "Triangular"
      1.88818
      (function (lambda (x) (if (< (abs x) 1)
                                (- 1 (abs x))
                                0)))
      1)
  (u  0.5
      0.3333333333333333
      0
      "Uniform"
      1.3510
      (function (lambda (x) (if (< (abs x) 1)
                                0.5
                                0)))
      1)
  (r  1.25
      0 
      falta
      "Order 4 kernel"
       2.01648
      (function (lambda (x) (if (< (abs x) 1)
                                (- (* 4.6875 (- 1 (sqr x)))
				   (* 3.28125 (- 1 (sqr (sqr x)))))
                                0)))
      1)
  (w  0.8158508158508158508
      0.11111111111111111111
      falta
      "Triweight"
      2.3122
      (function (lambda (x) (if (< (abs x) 1)
				 (* 1.09375 (expt (- 1 (sqr x)) 3))
				0)))
      1)
))

;some functions for accessing this structure

(defun funcall-kernel (type x 
			    &optional width 
			    (lambda-expr (nth 6 (assoc type *kernel-data*))))
  (when width (setf x (/ x width)))
  (if (listp x)
      (setf x 
	    (mapcar (eval lambda-expr) x))
      (setf x 
	    (funcall (eval lambda-expr) x)))
  (if width
      (/ x width)
    x))

;; kde-proto method :kernel-data also access this structure.


(provide "calckde");;;;;;
                   ;;;;;;;;;;;;;;;;;;;;;;;;;

;eof
