; file plotline.lsp (plot-lines.lsp, damn 8 dos char limit)
; f. udina,  dec 92
;
; contains some improvements to standard xlisp-stat plot-lines

;;;to be improved:
#|
define a pl-graph-proto object with all the gadgets 
add to it also an item menu for drawing gnuplot files
and others...

|#


(defproto pl-graph-proto () () graph-proto)
 
(defmeth pl-graph-proto :isnew (&rest args)
  (apply #'call-next-method args)
  (send self :menu-template '(RESCALE  DASH MOUSE DASH OPTIONS TO-GNUPLOT))
  (send self :new-menu)
  (dolist (mode (butlast (send self :mouse-modes)))
	  (send self :delete-mouse-mode mode))

  (send self :add-mouse-mode 'show-coordinates 
	:title "Show Coordinates" :cursor 'finger 
	:click :do-show-coordinates)
  (send self :add-mouse-mode 'zoom-in 
          :title "Zoom in" :cursor 'cross 
          :click :do-zoom-in)
  (send self :add-mouse-mode 'zoom-out 
	:title "Zoom out" :cursor 'cross 
	:click :adjust-to-data)
  
  (send self :mouse-mode 'show-coordinates)
  (let ((ov (send pl-overlay-proto :new)))
    (send self :margin 0 0 0 (+ 10 (send self :text-ascent)))
    (send self :add-overlay ov)
    (send ov :resize))

)

(defmeth pl-graph-proto :make-menu-item (item-template)
  (let (item)
    (setq item (case item-template
		     (to-gnuplot
		      (send graph-item-proto :new "Graph to Gnuplot file"
			    self :to-gnuplot))))
    (unless item
	    (setq item (call-next-method item-template)))
    item))


;methods for new mouse-modes
(defmeth pl-graph-proto :do-show-coordinates (x y m1 m2)
  (let* ((xy (cond (m1 (list x y))
                   (m2 (send self :canvas-to-scaled x y))
                   (t (send self :canvas-to-real x y))
                   ))
	 (s (format nil "~s" xy))
	 (str-size (send self :text-width s))
	 (left (> (+ x str-size) (send self :canvas-width)))
	 (horz (if left 2 0))
	 (vert 0)
	 (mode (send self :draw-mode)))
    (send self :draw-mode 'xor)
    (send self :draw-text s x y horz vert)
    (send self :while-button-down #'(lambda (x y) nil))
    (send self :draw-text s x y horz vert)
    (send self :draw-mode mode)))


(defmeth pl-graph-proto :do-show-coordinates (x y m1 m2)
;modified from graphics.lsp, xlispstat for mac
;this version allows moving the mouse while seeing the coordinates
;of the click point
  (let* ((xy (cond (m1 (list x y))
                   (m2 (send self :canvas-to-scaled x y))
                   (t (send self :canvas-to-real x y))
               ))
         (s (format nil "~,4g ~,4g" (first xy) (second xy)))
         (str-size (send self :text-width s))
         (left (> (+ x str-size) (send self :canvas-width)))
         (horz (if left 2 0))
         (vert 0))
    (send self :draw-string-while-button s x y horz vert)))

(defmeth pl-graph-proto :draw-string-while-button (s x y h v)
  (let* ((oldx x)
         (oldy y)
         (origin (first (transpose (send self :scaled-range '(0 1)))))
         (origin (apply #'send self :scaled-to-canvas origin))
         (mode (send self :draw-mode)))
    (send self :draw-mode 'xor)
    (send self :draw-line x y (first origin) y)
    (send self :draw-line x y x (second origin))
    (send self :draw-text s x y h v)
    (send self :while-button-down
          #'(lambda (nx ny)
              (send self :draw-text s oldx oldy h v)
              (send self :draw-text s nx ny h v)
              (setq oldx nx oldy ny)))
    ;redraw things for erasing
    (send self :draw-text s oldx oldy h v)
    (send self :draw-line x y (first origin) y)
    (send self :draw-line x y x (second origin))
    (send self :draw-mode mode)))


(defmeth pl-graph-proto :frame-rect (l2 t2 w2 h2)
  (when (< w2 0) (setq w2 (- w2)) (setq l2 (- l2 w2)))
  (when (< h2 0) (setq h2 (- h2)) (setq t2 (- t2 h2)))
  (call-next-method l2 t2 w2 h2))

(defmeth pl-graph-proto :do-zoom-in (x y m1 m2)
  (let* ((oldx x)
	 (oldy y)
	 (old-draw-mode (send self :draw-mode))
	 (rect (progn
		 (send self 
		       :while-button-down
		       #'(lambda (nx ny)
			   (send self :draw-mode 'xor)
			   (send self :frame-rect x y
				 (- oldx x) (- oldy y))
			   (send self :frame-rect x y
				 (- nx x) (- ny y))
			   (setf oldx nx)
			   (setf oldy ny)))
		 (list x y (- oldx x) (- oldy y))))
	 (l (nth 0 rect))
	 (to (nth 1 rect))
	 (w (nth 2 rect))
	 (h (nth 3 rect))
	 (lb (send self :canvas-to-scaled l (+ to h)))
	 (tr (send self :canvas-to-scaled (+ l w) to))
	 (xmin (nth 0 lb))
	 (xmax (nth 0 tr))
	 (ymin (nth 1 lb))
	 (ymax (nth 1 tr)))
    (send self :frame-rect l to w h)
    (send self :draw-mode old-draw-mode)
    (send self :set-ranges (min xmin xmax) (max xmin xmax)
	  (min ymin ymax) (max ymin ymax))))

(defmeth pl-graph-proto :do-zoom-in (x y m1 m2)
(if (or m1 m2)
  (send self :adjust-to-data)
  (let* ((oldx x)
         (oldy y)
         (old-draw-mode (send self :draw-mode))
         (rect (progn (send self 
                            :while-button-down
                            #'(lambda (nx ny)
                                (send self :draw-mode 'xor)
                                (send self :frame-rect x y
                                      (- oldx x) (- oldy y))
                                (send self :frame-rect x y
                                      (- nx x) (- ny y))
                                (setf oldx nx)
                                (setf oldy ny)))
                      (list x y (- oldx x) (- oldy y))))
         (l (nth 0 rect))
         (to (nth 1 rect))
         (w (nth 2 rect))
         (h (nth 3 rect))
         (lb (send self :canvas-to-scaled l (+ to h)))
         (tr (send self :canvas-to-scaled (+ l w) to))
         (xmin (nth 0 lb))
         (xmax (nth 0 tr))
         (ymin (nth 1 lb))
         (ymax (nth 1 tr)))
    (send self :frame-rect l to w h)
    (send self :draw-mode old-draw-mode)
    (send self :set-ranges (min xmin xmax) (max xmin xmax)
          (min ymin ymax) (max ymin ymax)))))

(defmeth pl-graph-proto :set-ranges (xmin xmax ymin ymax &key (recalc nil))
  (send self :scaled-range 0 xmin xmax :draw nil)
  (send self :scaled-range 1 ymin ymax :draw nil)
  (when recalc ()) ;this is to be done...................
  (unless (featurep :macintosh)
	  (send self :redraw))
  (send self :update nil))


(defun my-plot-lines (x y &rest args)
"args x y &rest args
 x and y are lists of coordinates, y can be a list of two lists
 args are for graph-proto :new"
  (let ((obj (apply #'send  pl-graph-proto :new 2 :show nil args)))
    (send obj :menu-title "Lines")
    (if (listp (first y))
	(progn (send obj :add-lines (list x (nth 0 y)))
	       (send obj :add-lines (list x (nth 1 y)) :type 'dashed))
      (send obj :add-lines (list x y)))
   
    (unless (featurep :macintosh)
	    (send obj :size 650 400)
	    (send obj :back-color 'black)
	    (send obj :draw-color 'white))
    ;;updating the window
    (send obj :adjust-to-data)
    (send obj :x-axis t t 5)
    (send obj :y-axis t t 4)
    (send obj :update nil)
    (send obj :redraw)
    (send obj :show-window)
    obj))

(defun my-plot-points (x y &key (title "Scatter Plot") variable-labels point-labels symbol color)
 "args x y &key...
 x and y are lists of coordinates"
 (let ((obj (send  pl-graph-proto :new 2 :show nil)))
   (send obj :menu-title "Points")
   (send obj :add-points (list x y) :point-labels point-labels :draw nil)
   
   (unless (featurep :macintosh)
	   (send obj :size 650 400)
	   (send obj :back-color 'black)
	   (send obj :draw-color 'white))
   ;;updating the window
   (send obj :adjust-to-data)
   (send obj :x-axis t t 5)
   (send obj :y-axis t t 4)
   (send obj :update nil)
   (send obj :redraw)
   (send obj :show-window)
   obj))




;;;;;;;;;;;;;;;;;;;;
;;; pl-overlay-proto
;;; borrowed from spin-control-overlay-proto
;;; must be finished...

(defproto pl-overlay-proto 
          '(top lefts gap side ascent box-top text-base)
          ()
          graph-overlay-proto)

(defmeth pl-overlay-proto :isnew ()
  (setf (slot-value 'gap) 4)
  (setf (slot-value 'side) 9)
  (setf (slot-value 'ascent) (send graph-proto :text-ascent))
  (let ((w1 (send graph-proto :text-width "Show coords."))
        (w2 (send graph-proto :text-width "Zoom in"))
        (w3 (send graph-proto :text-width "Zoom out"))
        (w4 (send graph-proto :text-width "not/av"))
        (w5 (send graph-proto :text-width "not/av"))
        (gap (slot-value 'gap))
        (side (slot-value 'side)))
    (setf (slot-value 'lefts)
          (list gap
                (+ (* 2 gap) side)
                (+ (* 5 gap) (* 2 side) w1)
                (+ (* 6 gap) (* 3 side) w1)
                (+ (* 9 gap) (* 4 side) w1 w2)
                (+ (* 10 gap) (* 5 side) w1 w2)
                (+ (* 13 gap) (* 6 side) w1 w2 w3)
                (+ (* 16 gap) (* 7 side) w1 w2 w3 w4)
                ))))

(defmeth pl-overlay-proto :resize ()
  (let* ((graph (send self :graph))
         (height (send graph :canvas-height))
         (bottom-margin (fourth (send graph :margin)))
         (top (+ (- height bottom-margin) 1))
         (gap (slot-value 'gap))
         (side (slot-value 'side))
         (ascent (send graph :text-ascent))
         (text-base (+ top gap (max side ascent)))
         (box-top (- text-base side)))
    (setf (slot-value 'top) top)
    (setf (slot-value 'text-base) text-base)
    (setf (slot-value 'box-top) box-top)))

(defmeth pl-overlay-proto :redraw ()
  (let ((graph (slot-value 'graph))
        (top (slot-value 'top))
        (lefts (slot-value 'lefts))
        (gap (slot-value 'gap))
        (side (slot-value 'side))
        (text-base (slot-value 'text-base))
        (box-top (slot-value 'box-top)))
    (send graph :draw-line 0 top (send graph :canvas-width) top)
    (mapcar #'(lambda (x) (send graph :erase-rect x box-top side side))
            (select lefts '(1 3 5 6 7)))
    (mapcar #'(lambda (x) (send graph :frame-rect x box-top side side))
            (select lefts '(1 3 5 6 7)))
    (case (send graph :mouse-mode)
      ('show-coordinates 
       (funcall #'(lambda (x) (send graph :paint-rect x box-top side side))
              (nth 1 lefts)))
      ('zoom-in 
       (funcall #'(lambda (x) (send graph :paint-rect x box-top side side))
              (nth 3 lefts)))
      ('zoom-out 
       (funcall #'(lambda (x) (send graph :paint-rect x box-top side side))
              (nth 5 lefts))))

    (mapcar #'(lambda (s x y) (send graph :draw-string s x y))
            '("Show coords." "Zoom in" "Zoom out" "not/av" "not/av")
            (+ (select lefts '(1 3 5 6 7)) gap side) 
            (repeat text-base 5))))


(defmeth pl-overlay-proto :do-click (x y m1 m2)
  (let ((graph (slot-value 'graph))
        (top (slot-value 'top))
        (lefts (slot-value 'lefts))
        (gap (slot-value 'gap))
        (side (slot-value 'side))
        (text-base (slot-value 'text-base))
        (box-top (slot-value 'box-top))
        )
    (when (and (< top y) (< x (+ side (nth 7 lefts))))
          (when (and (< top y) (< x (nth 6 lefts)))
                (send graph :idle-on nil)
                (if (< box-top y text-base)
                    (let ((i (car (which (< lefts x (+ lefts side))))))
                      (when i
                            (send graph :mouse-mode 
                                  (select '(show-coordinates 
                                            zoom-in zoom-out nil nil)
                                          (floor (/ i 2))))
                            (send self :redraw)))))
          t)))
  


;;;;;;some test functions

(defun mk () (setf plot (plot-lines 
			 (sort (normal-rand 50) #'<) (normal-rand 50)
			 :title "plot: a lines plot")))



(provide "my-plot-lines")
