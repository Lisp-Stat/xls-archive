#|

blin-tpp.lsp

(C) Darren J Wilkinson

This file may be distributed in unmodified form, and may be modified
for personal use. It may not be distributed in modified form.
Suggested modifications should be sent to me for inclusion in the
next release.

d.j.wilkinson@newcastle.ac.uk
http://www.ncl.ac.uk/~ndjw1/

|#

(provide "blin-tpp")

;; tree plotting stuff

;; TO BE COMPLETELY RE-HASHED!!!!!!!!!!!!!!!









;; set up a prototype for plotting junction trees
(defproto tree-plot-proto
  '(nodes resolutions size-ratios)
  nil
  graph-window-proto)

;; now define some methods for the prototype

;; method to graph current info from the tree
(defmeth tree-plot-proto :grab-info ()
  (dolist (node (slot-value 'nodes))
	  (let* ((pos (position node (slot-value 'nodes)))
		 (res (select (slot-value 'resolutions) pos))
		 (sr  (select (slot-value 'size-ratios) pos))
		 (addres (send (symbol-value node) :resolution))
		 (addsr (send (symbol-value node) :size-ratio))
		 (newres (cons addres res))
		 (newsr (cons addsr sr))
		 )
	    (setf (select (slot-value 'resolutions) pos) newres)
	    (setf (select (slot-value 'size-ratios) pos) newsr)
	    )
	  )
  (send self :redraw)
)

;; plot method
(defmeth tree-plot-proto :plot ()
  (dolist (node (slot-value 'nodes))
	  (send self :plot-arcs node)
	  )
  (dolist (node (slot-value 'nodes))
	  (send self :plot-node node)
  )
)

;; node plotting method
(defmeth tree-plot-proto :plot-node (node)
  (let* ((loc (send (symbol-value node) :location))
	 (x (round (* (first loc) (send self :scale))))
	 (y (round (* (second loc) (send self :scale))))
	 (d (send self :node-disp))
	 (res (select (slot-value 'resolutions) (position node (slot-value 'nodes))))
	 (cumres (reverse (cumsum (reverse res))))
	 )
;; first draw the basic node outline
    (send self :draw-color 'node-back)
    (send self :paint-oval (- x d) (- y d) (* 2 d) (* 2 d))
    (send self :draw-color 'outline)
    (send self :line-width 1)
;    (send self :frame-oval (- x d) (- y d) (* 2 d) (* 2 d))
;; next shade the resolutions
    (dotimes (i (length cumres))
      (let* ((thisres (select cumres i))
	     (disp (round (* (sqrt thisres) d)))
	     )
	(send self :draw-color (intern (format nil "~a" i)))
	(send self :paint-oval (- x disp) (- y disp) (* 2 disp) (* 2 disp))
	(send self :draw-color 'black)
	(send self :frame-oval (- x disp) (- y disp) (* 2 disp) (* 2 disp))
	)
      )
 )
)

;; arc plotting method
(defmeth tree-plot-proto :plot-arcs (node)
  (let* ((loc1 (send (symbol-value node) :location))
	 (x1 (round (* (first loc1) (send self :scale))))
	 (y1 (round (* (second loc1) (send self :scale))))
	 (neighbours (send (symbol-value node) :slot-value 'neighbours))
	 )
    (dolist (node2 neighbours)
	    (let* ((loc2 (send (symbol-value node2) :location))
		   (x2 (round (* (first loc2) (send self :scale))))
		   (y2 (round (* (second loc2) (send self :scale))))
		   )
	      (send self :draw-color 'arc)
	      (send self :line-width 3)
	      (send self :draw-line x1 y1 x2 y2)
	      )
	    ))
)

;; scale method
(defmeth tree-plot-proto :scale ()
  (min (send self :size))
)

;; node-disp method
(defmeth tree-plot-proto :node-disp ()
  (round (/ (send self :scale) 2 (length (slot-value 'nodes))))
)

;; redraw
(defmeth tree-plot-proto :redraw ()
  (send self :plot)
)


;; ********************************************************************
