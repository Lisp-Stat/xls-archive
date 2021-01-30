;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; file kde-add-point.lsp or kdeaddpt.lsp (damn DOS)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; slight modification to kde object to show how kernel density estimation
; is defined and computed
; f. udina. jan 94.
; 

;;;this file introduces slight modifications to kde-proto and his window
;wkde-proto to allow data creation with the mouse.
;once this file is loaded, new kde objects created will have a new mouse mode
; When the mouse mode 'Add data points' is selected, clicking with the mouse
; will add data to the kde object, and a little segment will be drawn for 
; each data point.

(require "runkde")
(require "kde")
(require "wkde")


;;;all modifications will be done to global variables kde and wkde
;; redefine it if do you want to
(setq playkde (make-kde :data (- (uniform-rand 3) 0.5) 
                    :x-values (rseq -1 1 100)
                    :calc-method 'direct
                    :title "Play KDE"))
(setq wplaykde (send playkde :slot-value 'window))



;;adding add-point mouse-mode to wplaykde-proto

(send wplaykde  :add-mouse-mode 'add-data-point 
	:title "Add data points" :cursor 'finger 
	:click :do-add-point)

(defmeth wplaykde :do-add-point (x y m1 m2)
;modified from graphics.lsp, xlispstat for mac
;this version allows moving the mouse while seeing the coordinates
;of the click point
  (let* ((xy (cond (m1 (list x y))
                   (m2 (send self :canvas-to-scaled x y))
                   (t (send self :canvas-to-real x y))
               ))
         (x (first xy))
         (kde (slot-value 'kde-core))
         (dt (send kde :data))
         (xv (send kde :x-values)))
    (send kde :data (append (list x) dt) nil)
;    (send kde :x-values xv)
    (send kde :redraw-window)
    (send kde :show-info-in-window)
))


;; modifying redraw-window for kde-proto

(defmeth playkde :redraw-window (&key (force nil) (clear t) (only-info nil))
  (unless only-info
    (if (send self :slot-value 'bootstrap-quant-lines)
        (send self :draw-bootstrap-quants)
        (progn
         (send self :draw-estimates :force force :clear clear)
         (when (and (slot-value 'window) 
                    (eql 'add-data-point (send self :to-window :mouse-mode)))
               (send self :draw-data-dots)))))
  (send self :to-window :redraw-content)
  (send self :draw-info-in-window))

(defmeth playkde :draw-data-dots ()
  (let* ((ww (slot-value 'window))
	 (y0 (second (send ww :real-to-canvas 0 0)))
         (y1 (second (send ww :canvas-to-scaled 0 (+ y0 2))))
         (y2 (second (send ww :canvas-to-scaled 0 (+ y0 12))))
         (dt (send self :data)))
    (send ww :add-lines (list (send ww :range 0) (list 0 0)))
    (mapcar #'(lambda (x)
                (send ww :add-lines (list (list x x)
                                                      (list y1 y2))))
            dt)))

(defmeth playkde :draw-kernel-sample ()
 (let* ((dt (send self :data))
        (sf (/ 1 (length dt))))
   (mapcar #'(lambda (p) (call-next-method p sf))
           dt)))


(send wplaykde :mouse-mode 'add-data-point)
(send playkde :toggle-show-a-kernel t)
(send playkde :redraw-window)
(send playkde :to-window :adjust-to-data)
(send playkde :redraw-window)


;;(list -0.5 -0.25 0 0.25 .5)

(format t "~%playkde and wplaykde store the objets created~%")


