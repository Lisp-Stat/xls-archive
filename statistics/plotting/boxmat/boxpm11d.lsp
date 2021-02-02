(defun boxmat-ct (data
		  &key like transpose reverse-x reverse-row
		       title new-x new-x-range margin
		       min max df
		       y-axis-label
		       y-axis-tick
		       x-axis-label
		       row-names
		       variable-labels
		 )
"Creates a new boxplot matrix object from the data, copying the
customization characteristics of the boxmat object referenced in
:like.	If the :tranpose argument is t, then both the data and
:like object are transposed.  If the reverse-x keyword is t, then
it effectively rotates the axes."

(if (and like (or x-axis-label row-names variable-labels))
    (error "incompatible arguments")
)


(cond
((not like)
       (boxmat data
	       :title (if title title "Boxplot Matrix")
	       :min  min
	       :max  max
	       :df   df
	       :x    new-x
	       :x-range new-x-range
	       :y-axis-label y-axis-label
	       :y-axis-tick   y-axis-tick
	       :margin (if margin margin '(0 -2 -35 -9))
	       :reverse-x reverse-x
	       :reverse-row reverse-row
	       :transpose transpose
	       :x-axis-label x-axis-label
	       :row-names row-names
	       :variable-labels (if variable-labels variable-labels
						    '("column" "row")
				)
       )
)

(t
(unless (if (objectp like)
	    (eql 'boxmat-proto (send like :slot-value 'proto-name))
	)
	(error ":like argument must be a boxplot matrix")
)

(let* (
       (b like)
       (variable-labels-b (send b :variable-label '(0 1)))
       (y-range-b (send b :slot-value 'y-range))
       (row-nam-b (send b :slot-value 'row-names))
       (isnrb (iseq (length row-nam-b)))
       (x-lab-b (send b :slot-value 'x-axis-label))
       (isncb (iseq (length x-lab-b)))
       (y-lab-b (send b :slot-value 'y-axis-label))
       (y-tick-b (send b :slot-value 'y-axis-tick))

       (x-seq		 t)
       (row-seq 	 t)
       (x-axis-label	 t)
       (row-names	 t)
       (variable-labels  t)
      )

      (cond (transpose
	     (setf x (if new-x new-x (1+ isnrb)))
	     (setf x-seq   (if reverse-row (reverse isncb) isncb))
	     (setf row-seq (if reverse-x   (reverse isnrb) isnrb))
	     (setf data (transpose (select data row-seq x-seq)))
	     (setf x-axis-label (if reverse-x (reverse row-nam-b) row-nam-b))
	     (setf row-names (if reverse-row (reverse x-lab-b) x-lab-b))
	     (setf variable-labels (reverse variable-labels-b))
	    )
	    (t
	     (setf x (if new-x new-x (1+ isncb)))
	     (setf x-seq   (if reverse-x   (reverse isncb) isncb))
	     (setf row-seq (if reverse-row (reverse isnrb) isnrb))
	     (setf data (select data row-seq x-seq))
	     (setf x-axis-label (if reverse-x (reverse x-lab-b) x-lab-b))
	     (setf row-names  (if reverse-row (reverse row-nam-b) row-nam-b))
	     (setf variable-labels variable-labels-b)
	    )
      )

      (boxmat data
		  :title (if title title (send b :title))
		  :mean-color (send b :slot-value 'mean-color)
		  :min	(if min min (select y-range-b 0))
		  :max	(if max max (select y-range-b 1))
		  :df	(if df	df  (send b :slot-value 'df))
		  :print-ss   (send b :slot-value 'print-ss)
		  :ss-where   (send b :slot-value 'ss-where)
		  :x	      x
		  :x-range (if new-x-range new-x-range (list(- (min x).5)(+ (max x).5)))
		  :x-label-move  (send b :slot-value 'x-label-move)
		  :x-axis-label-where (send b :slot-value 'x-axis-label-where)
		  :y-axis-label (if y-axis-label y-axis-label
						 (if (or min max) nil y-lab-b))
		  :y-axis-tick	(if y-axis-tick  y-axis-tick
						 (if (or min max) nil y-tick-b))
		  :y-label-move  (send b :slot-value 'y-label-move)
		  :black-on-white (send b :slot-value 'black-on-white)
		  :showing-labels (send b :slot-value 'showing-labels)
		  :margin (if margin margin (send b :margin))
		  :reverse-x reverse-x
		  :reverse-row reverse-row
		  :range-detail (send b :slot-value 'range-detail)
		  :transpose transpose
		  :x-axis-label x-axis-label
		  :row-names row-names
		  :variable-labels variable-labels
      )
)
)
)
)
