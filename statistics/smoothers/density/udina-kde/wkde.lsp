;;;file wkde.lsp
;;; wkde objects, are the window objects for kde objects
;;; separed from kdeobj.lsp on aug 25 93

;;different kde object can have different windows:
;; kde (unidim) object have wkde windows, as built in this file
;; bkde (bidim) objects will have two kind of windows (file wbkde.lsp)
;;   a graph-proto descendant for showing contour graphics
;;   and a spin-proto descendant for displaying full 3D surfaces
;; (setq outline-regexp "(setq outline-regexp "(def\\|;;;\\|(load\\|(auto\\|(requi")


#| metodes definits aqui:

(defproto wkde-proto '(kde-core) () graph-proto)         
(defmeth wkde-proto :to-core (&rest args)                
(defmeth wkde-proto :isnew (num &key (title nil) kde-core
(defmeth wkde-proto :redraw-content ()                   
(defmeth wkde-proto :close ()                            
(defmeth wkde-proto :draw-info-strings ()                
(defmeth wkde-proto :do-show-coordinates (x y m1 m2)     
(defmeth wkde-proto :draw-string-while-button (s x y h v)
(defmeth wkde-proto :do-calc-probability (x y m1 m2)     
(defmeth wkde-proto :do-zoom-in (x y m1 m2)              
(defmeth wkde-proto :set-ranges (xmin xmax ymin ymax &key
(defmeth wkde-proto :locate-dialog (dialog)
(defmeth wkde-proto :ask-save-image
(defmeth wkde-proto :make-menu-item 

|#

(defproto wkde-proto '(kde-core) () graph-proto)

(defmeth wkde-proto :to-core (&rest args)
  (if (null args)
      (slot-value 'kde-core)
      (apply #'send (slot-value 'kde-core) args)))

(defmeth wkde-proto :isnew (num &key (title nil) kde-core 
				(show nil) (debug nil))
;; &rest args)
  (apply #'call-next-method num :show nil :title title ())
  (setf (slot-value 'kde-core) kde-core)

  (send self :add-mouse-mode 'show-coordinates 
	:title "Show Coordinates" :cursor 'finger 
	:click :do-show-coordinates)
  (send self :add-mouse-mode 'zoom-in 
	:title "Zoom in" :cursor 'cross 
	:click :do-zoom-in)
  (send self :add-mouse-mode 'calc-probabilities 
	:title "Probability from...to..." :cursor 'cross 
	:click :do-calc-probability)  
  (send self :delete-mouse-mode 'selecting)
  (send self :delete-mouse-mode 'brushing)
  (send self :delete-mouse-mode 'point-moving)
  (send self :mouse-mode 'show-coordinates)

  (send self :margin 0 0 0 (* 3 (send self :text-ascent)))
  (send self :x-axis t t 5)
  (send self :y-axis t t 4)
  (send self :to-core :estimates-y)
  (send self :adjust-to-data :draw nil)
  (send self :update nil)
  
  (when show (send self :show-window)
	(send self :redraw-content)
	(send self :to-core :show-info-in-window)
	(send self :activate t);;in release 3 this is needed???
	)
  self
)

(defmeth wkde-proto :redraw-content ()
  (call-next-method)
  (when (send self :to-core :slot-value 'show-data-points)
	(send self :to-core :draw-data))
  (send self :draw-info-strings)
)

(defmeth wkde-proto :adjust-to-data (&key (draw t))
	(call-next-method :draw draw)
	(let ((rng (send self :range 1)))
	   (if (< (abs (first rng)) 0.0001)
	       (setf (first rng) 0))
	   (apply #'send self :range 1 rng)))

(defmeth wkde-proto :close ()
"Before closing its windows, closes other child windows"
(call-next-method)
(send self :to-core :slot-value 'window nil)
(send self :to-core :close-windows))

(defmeth wkde-proto :draw-info-strings ()
  (let* ((bot (nth 3 (send self :margin)))
	 (l 2)                                ;l top w h
	 (top (- (send self :canvas-height) bot 10))
	 (w (- (send self :canvas-width) 4))
	 (h (+ bot 8))
	 (incpos (+ (list 0 (send self :text-ascent))))
	 (pos (+ '(5 2) (list l top)))
	 (is (send self :to-core :slot-value 'info-strings))
	 (text1 (first is))
	 (text2 (second is))
	 (text3 (third is)))
    (send self :erase-rect l top w h)
    (send self :frame-rect l top w h)
    (setf pos (+ pos incpos))
    (when text1
      (send self :draw-text text1 (nth 0 pos) (nth 1 pos) 0 0))
    (setf pos (+ pos incpos))
    (when text2  
      (send self :draw-text text2 (nth 0 pos) (nth 1 pos) 0 0))
    (setf pos (+ pos incpos))
    (when text3
      (send self :draw-text text3 (nth 0 pos) (nth 1 pos) 0 0))))


(defmeth wkde-proto :do-show-coordinates (x y m1 m2)
;modified from graphics.lsp, xlispstat for mac
;this version allows moving the mouse while seeing the coordinates
;of the click point
  (let* ((xy (cond (m1 (list x y))
                   (m2 (send self :canvas-to-scaled x y))
                   (t (send self :canvas-to-real x y))
               ))
         (s (format nil "(~,3g, ~,3g)" (first xy) (second xy)))
         (str-size (send self :text-width s))
         (left (> (+ x str-size) (send self :canvas-width)))
         (horz (if left 2 0))
         (vert 0))
    (send self :draw-string-while-button s x y horz vert)))

(defmeth wkde-proto :draw-string-while-button (s x y h v)
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

(defmeth wkde-proto :do-calc-probability (x y m1 m2)
;when mouse-mode is selected, reads two x values from the mouse
; and then shows in windows the probabiity P(xmin<x<xmax)
  (let* ((xmin (first (send self :canvas-to-scaled x y)))
        (xmax xmin)
        (prob 0)
        (str nil))
    (send self :to-core :show-info-in-window
          (format nil "from ~,3g" xmin)
	  "drag mouse to to-value")
    (send self :while-button-down
          #'(lambda (nx ny)
              (setq xmax (first (send self :canvas-to-scaled nx ny)))
              (send self :to-core
                    :show-info-in-window
		    (format nil "from ~,3g to ~,3g" xmin xmax))))
    (setq prob (send self :to-core :calc-probability-between
                     (min xmin xmax) (max xmin xmax)))
    (send self :to-core
          :show-info-in-window
          (format nil  "from ~,3g" xmin)
          (format nil  "  to ~,3g" xmax)
          (format nil  "probability, approx: ~,4g" prob))))

(defmeth wkde-proto :do-zoom-in (x y m1 m2)
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


(defmeth wkde-proto :set-ranges (xmin xmax ymin ymax &key (recalc nil))
  (send self :scaled-range 0 xmin xmax :draw nil)
  (send self :scaled-range 1 ymin ymax :draw nil)
  (when recalc (error "set-ranges does not recalculate" nil)) ;this is to be done...
  (unless (featurep :macintosh) (send self :redraw))
  (send self :update nil))

(defmeth wkde-proto :locate-dialog (dialog)
#-msdos"will return a good position for a dialog to appear"
#+msdos"this method does nothing in MS-Windows"
;it tries to locate first in rigth
;then in down
;and if all fails, over left-up corner of myself
#+msdos()
#-msdos(let* ((mypos (send self :location))
       (dlgsize (if (send dialog :size)
		    (send dialog :size)
		  '(150 150)))
       (screen (screen-size))
       (mysize (send self :size))
       
       (all (- screen (+ mypos mysize dlgsize))))
  (cond ((> (first all) 0)
	 (apply #'send dialog :location (list (+ (first mypos)
						 (first mysize))
					      (second mypos))))
	((> (second all) 0)
	 (apply #'send dialog :location (list (first mypos)
					      (+ (second mypos)
						 (second mysize)))))
	(t (apply #'send dialog :location (+ '(10 20) mypos))))))


(defmeth wkde-proto :ask-save-image (&optional (file "image.ps" fset))
  (if (featurep :X11)
      (let ((bkc (send self :back-color))
	    (fgc (send self :draw-color)))
	(send self  :back-color 'white)
	(send self  :draw-color 'black)
	(send self :redraw)
	(if fset (call-next-method file)
	  (call-next-method ))
	(send self  :back-color bkc)
	(send self  :draw-color fgc)
	(send self :redraw))
    (message-dialog "Only for X11 version")))
    

(defmeth wkde-proto :make-menu-item (item-template)
  (let ((it (call-next-method item-template)))
    (if it it
      (send self :to-core :make-menu-item item-template))))

(defmeth wkde-proto :do-key (char shift option)
"some keyboard interface"
(when  *kde-debugging* (print (list char (char-int char) shift option)))
(case char
      (#\+
       (send self :to-core :bandwidth (* 1.5 (send self :to-core :bandwidth)))
       (send self :to-core :show-info-in-window nil "" ""))
      (#\-
       (send self :to-core :bandwidth (/ (send self :to-core :bandwidth) 1.5))
       (send self :to-core :show-info-in-window nil "" ""))
      (#\d (send self :to-core :toggle-show-data-points))
      (#\b (send self :to-core :draw-boxplot))
      (#\h (send self :to-core :pop-histogram-slider))
      (#\p 
       (setq *kde*poly* (not *kde*poly*))
       (send self :to-core :redraw-window))
      (#\a 
       (send self :adjust-to-data)
       (send self :to-core :redraw-window))
      (#\k (send self :to-core :toggle-show-a-kernel))
      #+Macintosh ((#\r #\) (send self :to-core :histogram-shift :percent 1))
      #+Macintosh ((#\l #\) (send self :to-core :histogram-shift :percent -1))
      #-Macintosh ((#\r) (send self :to-core :histogram-shift :percent 1))
      #-Macintosh ((#\l) (send self :to-core :histogram-shift :percent -1))
      ))

(provide "wkde")
