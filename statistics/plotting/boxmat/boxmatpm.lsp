;(load "boxpm11a")  ; boxmat-proto boxmat
;(load "boxpm11b")  ; :redraw
;(load "boxpm11c")  ; :add-boxplot replaced by this file
;(load "boxpm11d")  ; needed for boxmat transpose

(send boxmat-proto :slot-value 'lines-per-box 4)

(defmeth boxmat-proto :add-boxplot (y &key (x 1.0) (width 1.0) (draw nil))
  (unless (= 2 (send self :num-variables)) (error "only works for 2D plots"))
  (let* ((half-box (* 0.4 width))
	 (q1 (min y))
	 (q3 (max y))
	 (num-lines  (send self :num-lines))
	)
    (setf (slot-value 'box-line-start)
		      (concatenate 'list (slot-value 'box-line-start)
					 (list num-lines )))

    (send self :add-lines (+ x (* '(-1 -1 1 1) half-box))
			  (list q1 q3 q3 q1)
			  :draw nil
    )
    (send self :linestart-next (+ num-lines 3) num-lines)
  )
)

