;; reimplement :REDRAW-CONTENT, :DRAW-DATA-LINES :DRAW-DATA-POINTS & :REDRAW-BACKGROUND
;;  and adds :REDRAW-AXIS which is built into the C implementation of REDRAW-BACKGROUND


( provide "basicplt" )

(defproto basic-plot-proto () () scatterplot-proto)



#| 
	NOTE: the standard redraw order is :redraw-data-points THEN :redraw-data-lines 
    however, the reverse is actually better if you are mixing lines and 
    point data -- unless you have a very dense cluster of points, the 
    points over lines hides the lines less than lines over points obscures
    the points. This implementation uses points over lines -- if you need 
    the opposite behavior, you can swap the order of those lines below.
|# 


(defmeth basic-plot-proto :redraw-content ()
  (let ((cv (send self :current-variables))
         (nl (send self :num-lines))
         (np (send self :num-points)))
    (when (> nl 0) 
           (send self :draw-data-lines (first cv) (second cv) 0 (1- nl)))
    (when (> np 0) 
           (send self :draw-data-points (first cv) (second cv) 0 (1- np)))
;; actually, this (below) seems to be done in draw-data-points
    (when (and (send self :showing-labels) 
                 (send self :has-method :draw-data-strings)) ; NOT IMPL - thus the check 
           (send self :draw-data-strings))))



#| 
  :NUM-LINES is NOT actually number-of-lines, but number-of-line-(starts/ends) !
   [#1] fencepost error: 19 connected lines are defined with 20 points.
   [#2] not all linestarts are connected -- if :LINESTART-NEXT Index is Nil, 
        then that endpoint is not connected to the next endpoint. 
   ( Note that checking for case #2 also takes care of case #1 -- the last 
     point should usually return Nil for :LINESTART-NEXT ) 
|# 

;; none of the clipping in the C code is included here
;; much of error checking is included, but may not be necessary 
;; as lisp will signal and error rather than walk off a cliff.

(defmeth basic-plot-proto :draw-data-lines (var1 var2 m n)
  (let 	((nv (send self :num-variables))
         (nl (send self :num-lines))
         (nj (iseq m n)))
;; check validity of args         
    (when 
      (or (not (< -1 var1 nv)) (not (< -1 var2 nv)))
      (error "Vars: ~A not between 0 and ~d (:num-variables)" 
              (list var2 var2) nv))
    (when 
      (or  (< m 0) (>= n nl))
      (error "index ~a out of range ~a" (list m n) (list 0 nl)))
      
    (let ((save-width (send self :line-width))		 ; save graphics state
          (save-color (send self :draw-color))
          (save-type (send self :line-type))
          (content-rect (send self :content-rect)))  ; to correct for origin

    (let ((widths 	(send self :linestart-width nj))
          (types 	(send self :linestart-type nj))
          (colors 	(send self :linestart-color nj))
          (usecolor (send self :use-color))
          (masked (send self :linestart-masked nj))
          (xs (+ (first content-rect)
            	 (send self :linestart-canvas-coordinate var1 nj)))
          (ys (- (fourth content-rect)  (- (second content-rect))
     	         (send self :linestart-canvas-coordinate var2 nj))))
      (dotimes (i (length nj))
      	( let (( nexti ( send self :linestart-next i)))
                (when (and nexti (not (elt masked i)))
                       (when (and usecolor (elt colors i)) 
                              (send self :draw-color (elt colors i)))
                       (send self :line-width (elt widths i))
                       (send self :line-type  (elt types i))
                       (send self :draw-line 
                              (elt xs i) (elt ys i)
                              (elt xs nexti) (elt ys nexti))))))
      (send self :line-width save-width)	; restore graphics state
      (send self :draw-color save-color)
      (send self :line-type save-type)))
  (values))


;; possible changes and optimizations:
;;   * remove some of the error checking (see above) 
;;   * pop items off list instead of using elt ? 
;;   * only call graphics state methods when value changes 
;;   * add clipping ? check to see if point in CLIP-RECT 
;;   * add screen-buffering ? 


( defmeth basic-plot-proto :draw-data-points ( var1 var2 m n )
  (let 	((nv (send self :num-variables))
         (np (send self :num-points))
         (nj (iseq m n)))
;; check validity of args         
    (when 
      (or (not (< -1 var1 nv)) (not (< -1 var2 nv)))
      (error "Vars: ~A not between 0 and ~d (:num-variables)" 
              (list var2 var2) nv))
    (when 
      (or  (< m 0) (>= n np))
      (error "index ~a out of range ~a" (list m n) (list 0 np)))

	(let ((save-color (send self :draw-color))
		  (content-rect (send self :content-rect)))
    (let (
          (colors 	(send self :point-color nj))
          (usecolor (send self :use-color))
          (showing (send self :point-showing nj))
          (symbols (send self :point-symbol nj))
          (histate (mapcar #'(lambda (x) (or (eq x 'SELECTED) (eq x 'HILITED)))
          			(send self :point-state nj)))
          (xs (+ (first content-rect)
            	 (send self :point-canvas-coordinate var1 nj)))
          (ys (- (fourth content-rect)  (- (second content-rect))
     	         (send self :point-canvas-coordinate var2 nj))))
      (dotimes (i (length nj))
      	( when ( elt showing i )
      		( when (and usecolor (elt colors i))
      			 ( send self :draw-color (elt colors i)))
      		( send self :draw-symbol 
      			( elt symbols i ) ( elt histate i ) ( elt xs i ) ( elt ys i )))))
	(send self :draw-color save-color))) ;; restore graphics state 
  (values))



( defmeth basic-plot-proto :redraw-axis ()
   (let   ((xrange (send self :range 0))
		   (yrange (send self :range 1))
		   (xaxis  (send self :x-axis))
		   (yaxis  (send self :y-axis)))
   (when (or (first xaxis) (first yaxis))
   	(let* ((rxs (rseq (first xrange) (second xrange) (third xaxis)))
   		   (rys (rseq (first yrange) (second yrange) (third yaxis)))
   		   (cxs (mapcar #'(lambda (x) (first (send self :real-to-canvas x 0)))
					rxs ))
		   (cys (mapcar #'(lambda (y) (second (send self :real-to-canvas 0 y)))
					rys ))
		   (cx0 (first cxs))
		   (cy0 (first cys))
		   (cxn (first (last cxs)))
		   (cyn (first (last cys)))
		   (lw (send self :line-width))
		   (c (send self :draw-color)))
		(if (and (send self :has-method :axis-color)
				 (send self :axis-color))
			(send self :draw-color (send self :axis-color)))
		(send self :line-width 2)  
		(when (first xaxis)
		 	(send self :draw-line cx0 cy0 cxn cy0)
		 	( do ((x (pop cxs) (pop cxs))
		 	      (rx (pop rxs) (pop rxs)))
		 	     ((null x))
		 		(send self :draw-line x cy0 x (+ cy0 5))
		 		(send self :draw-text (format nil "~4f" rx)
		 				x (+ cy0 (send self :text-ascent))  1 1 )))		 			
		(when (first yaxis)
		 	(send self :draw-line cx0 cy0 cx0 cyn )
		 	( do ((y (pop cys) (pop cys))
		 	      (ry (pop rys) (pop rys)))
		 	     ((null y))
		 		(send self :draw-line cx0 y (- cx0 5) y)
		 		(send self :draw-text-up (format nil "~4f" ry)
		 			(+ 0 (send self :text-ascent)) y 1 1 )))
		(send self :line-width lw)
		(send self :draw-color c)))))



(defmeth basic-plot-proto :redraw-background ()
   (let ((dc (send self :draw-color)))
   	  (send self :draw-color (send self :back-color))
   	  (apply #'send self :paint-rect (send self :clip-rect))
   	  (send self :draw-color dc)
   	  (when (send self :has-method :grid-color)		;; backgrid mods included ? 
	  	(let (( color ( send self :grid-color )))
         	(if color  (send self :draw-grid :color color))
     	 	(if color  (send self :draw-zero-axis :color color))))
      (send self :draw-color dc)
   	  (send self :redraw-axis)))

		 