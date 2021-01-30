#|
Hi all,

Since the methods :to-gnuplot and :to-xfig work fine on two dimensional
graphic objects (graph-proto !), but I sometimes want to have good printout
of spin-proto objects, I wrote a little method (inspired by one of Weisberg's 
R-code routines) to extract 2d-plots (2 dim. graph-proto) from spin-proto.
It should copy: Point symbols, point colors, lines, line colors, 
selection, showing labels and axes.
Means you should ;-) get a one to one copy of the spin-plot in a 2 
dimensional plot.  To this 2 dimensional plot a method like :to-gnuplot 
or :to-xfig can then be sent.


regards

-- Bernhard
----------------------------------------------------------------------------
Bernhard       Walter                 walter@pollux.edv.agrar.tu-muenchen.de 
Techn. Univ. Muenchen   ######  #####           +49-(0)8161-71-5055  (phone)
DVS  -  Weihenstephan     #  #  # # #            +49-(0)8161-71-4409   (fax)
Hohenbachernstr.    2     #  #  # # #                              (Germany)
85350        Freising     #  #### # #                                       
----------------------------------------------------------------------------

--------8<--------8<--------8<--------8<--------8<--------8<--------8<------
|#

(defmeth spin-proto :extract-2d(&key (title "Spin-plot: 2D projecion"))
  (let* ((showing   (send self :points-showing))
	 (num-p     (send self :num-points))
	 (coords    (send self :point-transformed-coordinate '(0 1)
			       (list (iseq num-p) (iseq num-p))))
	 (x-range   (- (max (first  coords)) (min (first  coords))))
	 (y-range   (- (max (second coords)) (min (second coords))))
	 (x-fact    (/ (abs (apply #'- (send self :scaled-range 0))) x-range))
	 (y-fact    (/ (abs (apply #'- (send self :scaled-range 1))) y-range))
	 (fact      1)
	 (l-next  (if (=  (send self :num-lines) 0)
		      nil
		      (send self 
			    :linestart-next 
			    (iseq (send self :num-lines))))) 
	                    ; every NIL entry is start of a new line
	 (l-ends   (when l-next (which (mapcar #'not l-next))))
	 (l-starts (when l-next
			 (select (append '(0) (1+ l-ends))  
				 (iseq (length l-ends)))))  
	                    ; 0 is the first linestart remove last element

	 (2d-plot   (send graph-proto :new 2))
        )
    (send 2d-plot :title title)
    (send 2d-plot :use-color t)
    (send 2d-plot :showing-labels t)

    (mapcar #'(lambda(num coord-x coord-y)
		(let* ((color     (send self :point-color num))
		       (symbol    (send self :point-symbol num))
		      )
		  (send 2d-plot :add-points (list (list coord-x) 
						  (list coord-y)))
		  (send 2d-plot :point-label num (send self :point-label num))
		  (when color
			(send 2d-plot :point-color num color))
		  (when symbol
			(send 2d-plot :point-symbol num symbol))
		)
              )
	    (iseq num-p) (first coords) (second coords))

    (if (< (length showing) num-p)
	(progn
	 (send 2d-plot :selection showing)
	 (send 2d-plot :focus-on-selection)
	 (send 2d-plot :selection nil)
        ))
    (send 2d-plot :selection (send self :selection))

    (mapcar #'(lambda(l-s l-e)
		(let* ((coords (send self :linestart-transformed-coordinate
				          '(0 1) (list (iseq l-s l-e)
						       (iseq l-s l-e))))
		       (color  (send self :linestart-color l-s))
                      )
		  (if color
		      (send 2d-plot :add-lines 
			    (list (first coords) (second coords))
			    :color color)
		      (send 2d-plot :add-lines 
			    (list (first coords) (second coords)))
		  )
		)
	      )
	    l-starts l-ends)

    (apply #'send 2d-plot :range 0 (/ (send self :scaled-range 0) fact))
    (apply #'send 2d-plot :range 1 (/ (send self :scaled-range 1) fact))

    (when (send self :showing-axes)
	  (let* ((axes   (* 0.5 (max x-range y-range)
			    (if (send self :transformation)
				(row-list (send self :transformation))
			        '( (1 0 0) (0 1 0)))))
		 (center (butlast
			  (if (send self :transformation)
			      (coerce 
			       (matmult (send self :transformation) 
					(coerce (send self :center '(0 1 2))
						'vector))
			       'list)
			      (send self :center '(0 1 2))
                          )))
		 (num-p  (send 2d-plot :num-points))
                )
	    (mapcar #'(lambda(x y label)
			(send 2d-plot :add-points (list (list x)(list y))
			              :symbol 'DOT
				      :point-labels (list label))
			(send 2d-plot :selection 
                                      (append (send 2d-plot :selection)
	                                      (iseq num-p (+ num-p 2))))
			(send 2d-plot :add-lines (list (list 0 x) (list 0 y))
			              :type 'DASHED)
		      )
		    (coerce (first axes) 'list) (coerce (second axes) 'list)
		    '("X" "Y" "Z"))
	  ))
    2d-plot
  )
)

