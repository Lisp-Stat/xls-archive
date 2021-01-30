;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is file kdehisto.lsp, part of KDE
;;; F. Udina 1995
;;; Contains the methods for kde-proto
;;; dealing with histograms
;;;
;;; This file is loaded by kde.lsp, it does'nt work by itself.
#| Contains kde-proto methods:
     :histogram-num-bins (&optional (num 0 set))
     :sturge-rule ()
     :histogram-shift (&key (offset 0 oset) (percent 0 pset) (redraw t))
     :histogram-data (&key (recalc nil))
     :draw-histogram (&key (num-bins 0 nbset) (polygon *kde*poly*))
     :redraw-histogram (&optional num-bins method)
     :draw-boxplot (&key (y-pos nil))
|#

(defmeth kde-proto :histogram-num-bins (&optional (num 0 set))
"Sets or retrieves the slot. Default is curently computed by Sturge's rule.
 Called mainly from histogram dialog window."
   (when set ; if it really changes, set it and arrange things
	 (and (numberp (slot-value 'histogram-num-bins))
	      (/= (slot-value 'histogram-num-bins) num)
	      (send self :histogram-shift :offset 0)
	      (setf (slot-value 'histogram-num-bins) (round num))))
   (when ;if needed, apply the default and store it
    (or (not (slot-value 'histogram-num-bins))
	(= 0 (slot-value 'histogram-num-bins)))
    (setf (slot-value 'histogram-num-bins)
	  (send self :sturge-rule)))
   (slot-value 'histogram-num-bins))

(defmeth kde-proto :sturge-rule ()
"Default number of bins for the histogram, must be improved"
  (let* ((dt (send self :data))
	 (n (length dt))
	 (rg (apply #'- (select (send self :fivnums) '(4 0))))
	 (xv (send self :x-values))
	 (xrg (- (first (last xv)) (first xv)))
	 (sturg (+ 1 (/ (log n) (log 2))))
	 (wdth (/ rg sturg)))
    (round (/ xrg wdth))))

(defmeth kde-proto :histogram-shift (&key (offset 0 oset) (percent 0 pset) (redraw t))
"keys: offset percent (redraw t)
 Accessor to same name slot, that contains the amount of shifting
 to be applied to the histogram.
 offset is added to the minimum x value
 of the kde graph, and it will be the end of the first bin.
 When percent is given, offset is computed as percent of histo bin width"
(or (and pset
	 (/= 0 percent)
	 (slot-value 'histogram-slider)
	 (slot-value 'histogram-data)
	 (let* ((hd (first (send self :histogram-data)))
		(ww (- (second hd) (first hd)))
		(shft (send self :histogram-shift)))
	   (cond ((= percent 1); means increment 10%
		  (setf shft (* ww (+ 0.1 (/ shft ww)))))
		 ((= percent -1); means decrement 10%
		  (setf shft (* ww (- (/ shft ww) 0.1))))
		 ((< 0 percent 1)
		  (setf shft (* ww percent)))
		 (t (error "percent not valid in histogram-shift" percent)))
	   (send self :histogram-shift :offset shft :redraw redraw)))
    (when oset 
	  ;;reduce offset modulo histo width
	  (and (/= 0 offset)
	       (slot-value 'histogram-data)
	       (let* ((hd (first (send self :histogram-data)))
		      (ww (- (second hd) (first hd))))
		 (loop (if (> offset 0) (return))
		       (setf offset (+ offset ww)))
		 (loop (if (< offset ww)
			   (return))
		       (setf offset (- offset ww)))))
	  (setf (slot-value 'histogram-shift) offset)
	  (when (and redraw
		     (slot-value 'histogram-slider))
		(send self :redraw-histogram
		      (send self :histogram-num-bins)
		      (position (first (slot-value 'histogram-bin-method)) 
				'(direct linear))))))
(slot-value 'histogram-shift))


(defmeth kde-proto :histogram-data (&key (recalc nil))
"Retrieves slot :histogram-data. If needed, or if :recalc t,
 the slot is recalculated."
  (require "binning")
  (let* ((hd (if recalc
                 nil
                 (slot-value 'histogram-data)))
         (nb (send self :histogram-num-bins))
         (nb1 (+ nb 1))
         (dt (send self :data))
         (x-v (send self :x-values))
         xv  xvlen xrng xshft
         yv
	 (bc nil)); the histo bin counts
     (unless nb (setf hd nil))
    (when hd
       (setf xv (first hd))
       (setf yv (second hd))
       (unless (and (eql (if (= 0 (send self :histogram-shift)) nb nb1)
			 (- (length xv) 1))
		    (eql (if (= 0 (send self :histogram-shift)) nb nb1)
			 (length yv)))
          (setf hd nil)))
    (unless hd   ;recompute histo data
	    (setf xv (rseq (first x-v) (car (last x-v)) (+ nb 1)))
	    (setf xvlen (length xv))
	    (setf nb (1- xvlen))
	    (setf xshft (send self :histogram-shift))
	    (setf xrng (list (mean (select xv '(0 1)))
			     (mean (select xv
					   (list (- xvlen 2)
						 (1- xvlen))))))
	    (when (not (= 0 xshft)); another bin is needed here on the left
		  (when *kde*computing*inspect*
			(format t "shifting ~,4g (nb=~a) from (~,4g ,~,4g)" 
				xshft nb (first xrng) (second xrng)))
		  (setf xrng (+ xrng xshft))
		  (setf (first xrng)
			(- (first xrng)
			   (/ (- (apply #'- xrng)) nb)))
		  (when *kde*computing*inspect*
			(format t " to ~%(~,4g ,~,4g)"
				(first xrng) (second xrng)))
		  (setf nb (1+ nb)))
	    (setf bc (coerce
		      (bin-data dt
				:method (first (slot-value 'histogram-bin-method))
				:outsiders (second 
					    (slot-value 'histogram-bin-method))
				:xrange xrng
				:numbins nb)
		      'list))
	    (when (not (= 0 xshft));add the left bin
		  (setf xv (+ xshft 
			      (append (list (- (first xv)
					       (- (second xv) (first xv))))
				      xv))))
	    ;; hist data is not rescaled here,
            (setf yv bc)
	    (setf hd (list xv yv)))
    (setf (slot-value 'histogram-data) hd) ;return it
    ))


(defmeth kde-proto :draw-histogram (&key (num-bins 0 nbset) (polygon *kde*poly*))
"Called by draw-estimates if there is histogram-slider open"
    (let* ((nb (if nbset
                   (send self :histogram-num-bins num-bins)
                   (send self :histogram-num-bins)))
	   (nb (if (= 0 (send self :histogram-shift))
		   nb (1+ nb) ))
           (hd  (send self :histogram-data))
           (xvf (butlast (first hd)))
           (xvl (cdr (first hd)))
           (yv (second hd))
	   ;;yv must be rescaled:
           (maxy (max yv))
           ;;(topy (second (send self :to-window :range 1)))
	   ;;(yv (* yv (/ topy maxy) 0.9))
	   ;;let's rescale it to have area 1
	   (area (* (- (second xvf) (first xvf))
		    (sum yv)))
	   (yv (/ yv area)))
      (if polygon
	  (let (
		(dif (- (first xvl) (first xvf)))
		(xx (/  (+ xvf xvl) 2))
		(yy (append '(0) yv '(0)))
		)
;(break)
	    (setq xx (append (list (- (first xx) dif)) xx (+ dif (last xx))))
	    (apply #'send self :to-window :add-lines (list xx yy)
		   (if (system-has-color) 
		       (list :color 'red :draw nil)
		     (list :draw nil))))
	(let ((xx (list xvf xvf xvl xvl)) ; pure histogram
	      (yy (list (repeat 0 nb) yv yv (repeat 0 nb))))
	  (apply #'send self :to-window :add-lines
		 (list ( apply #'append (transpose xx))
		       (apply #'append (transpose yy)))
		 (if (system-has-color) 
		     (list :color 'red :draw nil)
		   (list :draw nil)))))))


(defmeth kde-proto :redraw-histogram (&optional num-bins method)
"forces recalc of the histogram. method is 0=direct, 1=linear"
(send self :histogram-num-bins num-bins)
(setf (first (slot-value 'histogram-bin-method))
      (select '(direct linear) method))
(send self :histogram-data :recalc t)
(send self :redraw-window))

(defmeth kde-proto :draw-boxplot (&key (y-pos nil))
  (when (slot-value 'window)
	(let* ((w (send self :to-window))
	       (y-pos (if y-pos y-pos
			(mean (send w :range 1))))
	       (yhi y-pos)
	       (ylo (* .94 yhi))
	       (yme (/ (+ yhi ylo) 2))
	       (fn (send self :fivnums))
	       (points
		(macrolet ((mkpoint (n y) `(list (nth ,n fn) ,y)))
			  (list (mkpoint 0 yme)
				(mkpoint 1 yme) (mkpoint 1 yhi)
				(mkpoint 2 yhi) (mkpoint 2 ylo) (mkpoint 2 yhi)
				(mkpoint 3 yhi) (mkpoint 3 yme)
				(mkpoint 4 yme)
				(mkpoint 3 yme) (mkpoint 3 ylo)
				(mkpoint 1 ylo) (mkpoint 1 yme)))))
	  (send w :add-lines (transpose points)))))

(provide "kdehisto")
