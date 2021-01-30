(defproto boxmat-proto
 '(row-centers row-names row-starts row-end local-range
   ss ms df print-ss ss-where
   box-line-start box-point-start lines-per-box mean-color
   box-out-start transpose reverse-x reverse-row
   x-range y-range margin cut-range range-detail
   x-axis-label x-axis-tick x-label-move x-axis-label-where
   y-axis-label y-axis-tick y-label-move)  ()
 scatterplot-proto
 "Boxplot matrix prototype."
)


(defun boxmat (data &key (title "Box Plot Matrix")
			 (variable-labels '("column" "row"))
			 mean-color
			 min
			 max
			 df
			 (margin '(0 -2 -35 -9))
			 (print-ss t)
			 (ss-where '(0 12 24))
			 x
			 x-range
			 x-axis-label
			 (x-label-move t)
			 (x-axis-label-where 17)
			 y-axis-label
			 y-axis-tick
			 (y-label-move t)
			 row-names
			 transpose
			 reverse-x
			 reverse-row
			 (cut-range 1)
			 (range-detail 20)
	      )
 "Args: (data &key (title \"Box Plot Matrix\")
		   (variable-labels '(\"column\" \"row\")))
 DATA is an array of sequences.  Makes an array of box plots
 of the sequences in the data array."
  (if (not (arrayp data)) (error "argument must be an array"))
  (let* ((p (send boxmat-proto :new 2
			       :title title
			       :show nil
			       :variable-labels variable-labels
			       :lines-per-box 9
			       :row-starts  '(0)
			       :row-centers '(0)
			       :local-range '(0 1)
			       :print-ss print-ss
			       :ss-where ss-where
			       :x-range x-range
			       :x-axis-label "0"
			       :x-axis-tick   0
			       :x-label-move  x-label-move
			       :x-axis-label-where x-axis-label-where
			       :y-axis-label "0"
			       :y-axis-tick   0
			       :y-label-move y-label-move
			       :cut-range cut-range
			       :range-detail range-detail
			       :transpose transpose
			       :reverse-x reverse-x
			       :reverse-row reverse-row
	    )
	 )

	 (nrows (array-dimension data 0))
	 (ncols (array-dimension data 1))
	 (width 1)
	 (non-nil-data	(remove nil (combine data)))
	 (min (if min (min non-nil-data min) (min non-nil-data)))
	 (max (if max (max non-nil-data max) (max non-nil-data)))
	 (extended-range (* (- max min) 1.1))
	 (local-range (get-nice-range min max range-detail))
	 (row-start (/ (- (+ min max) extended-range) 2))
	 (row-end (+ (* extended-range nrows) row-start))
	 (row-starts (+ (* (- nrows (iseq 1 nrows)) extended-range)
			row-start))
	 (row-centers (+ row-starts (/ extended-range 2)))
	)
	(send p :slot-value 'row-centers row-centers)
	(send p :slot-value 'row-starts row-starts)
	(send p :slot-value 'row-end row-end)
	(send p :slot-value 'local-range local-range)
	(send p :slot-value 'y-range (list min max))
	(send p :range 1 row-start row-end)
	(send p :y-axis t (not y-label-move) 0)
	(apply #'send p :margin margin)
	(send p :slot-value 'row-names (if row-names row-names (iseq 1 nrows)))
	(send p :slot-value 'mean-color
			    (if mean-color mean-color (send p :back-color)))
	(send p :use-color t)

	;box-axis-label-tick
	(let* ((row-starts-adjusted (- row-starts (first (last row-starts))))
	       (yt (select (if y-axis-tick y-axis-tick local-range) '(0 1)))
	       (tick-y (combine
			(outer-product (reverse row-starts-adjusted) yt #'+)))
	       (tick-label
		   (select (combine (if y-axis-label y-axis-label yt)
				    (repeat "" (* 2 nrows)))
			   (iseq 0 (1- (* 2 nrows))))
	       )
	      )
	      (send p :slot-value 'y-axis-tick  tick-y)
	      (send p :slot-value 'y-axis-label tick-label)
	)
	(if (= (length x-range) 2) (setf x-range (append x-range ncols)))
	(cond (x
		     (if (/= (length x) ncols)
			 (error "parameter :x wrong length."))
		     (setf width (min (difference x)))
		     (if (not x-range)
			 (setf x-range (get-nice-range (- (min x) width)
						       (+ (max x) width) 4))
		     )
		     (send p :range 0 (nth 0 x-range) (nth 1 x-range))
		     (send p :x-axis t (not x-label-move)
				       (if x-axis-label 0 (nth 2 x-range)))
		     (cond (x-axis-label
			    (send p :slot-value 'x-axis-tick x)
			    (send p :slot-value 'x-axis-label x-axis-label)
			   )
		     )
	      )
	      (t
		     (setf x (iseq 1 ncols))
		     (if (not x-range)
			 (setf x-range (list 0.5 (+ ncols 0.5) ncols))
		     )
		     (send p :range 0 (nth 0 x-range) (nth 1 x-range))
		     (send p :x-axis t (not x-label-move) 0)
		     (send p :slot-value 'x-axis-tick x)
		     (send p :slot-value 'x-axis-label (if x-axis-label x-axis-label x))
	      )
	)

	(let* ((ss (sum (^ (- non-nil-data (mean non-nil-data)) 2)))
	       (ms (flet ( (div-nil (a b) (if (and a b (/= b 0)) (/ a b))) )
			 (div-nil ss df)))
	      )
	      (send p :slot-value 'ss ss)
	      (send p :slot-value 'ms ms)
	      (send p :slot-value 'df df)
	)

	(map-elements #'send p :plotline
		     (nth 0 x-range) row-starts (nth 1 x-range) row-starts nil)
	(send p :plotline (nth 0 x-range) row-end (nth 1 x-range) row-end nil)
	(send p :plotline (nth 1 x-range) row-start (nth 1 x-range) row-end nil)
	(let* ((rs-adj (- row-starts (min row-starts)))
	       (rs (matrix (list nrows ncols) (repeat rs-adj (repeat ncols nrows))))
	       (xs (matrix (list nrows ncols) (repeat x nrows)))
	       (x-seq	(if reverse-x	(reverse (iseq ncols)) (iseq ncols)))
	       (row-seq (if reverse-row (reverse (iseq nrows)) (iseq nrows)))
	      )
	      (setf data (select data row-seq x-seq))
	      (setf rs	 (select rs   row-seq x-seq))
	      (setf xs	 (select xs   row-seq x-seq))
	      (cond (transpose
		     (setf data (transpose data))
		     (setf rs	(transpose rs))
		     (setf xs	(transpose xs))
	      ))
	      (let* ((rc (* nrows ncols))
		     (data-v (make-array rc :displaced-to data))
		     (rs-v   (make-array rc :displaced-to rs  ))
		     (xs-v   (make-array rc :displaced-to xs  ))
		     (data-i 1)
		     (rs-i 1)
		     (xs-i 1)
		    )
		    (dotimes (i rc)
		      (setf data-i (select data-v i))
		      (setf rs-i (select rs-v i))
		      (setf xs-i (select xs-v i))
		      (if (> (length data-i) 0)
			  (send p :add-boxplot
				  (+ data-i rs-i) :width width :x xs-i)
		      )
		    )
	      )
	)

	(send p :show-window)
	p
  )
)

