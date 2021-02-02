;;;
;;;  plot control prototypes
;;;

(defmeth graph-proto :add-control (c) (send self :add-overlay c))
(defmeth graph-proto :delete-control (c) (send self :delete-overlay c))

;;; graph control proto
;;; written by Luke Tierney

(defproto graph-control-proto 
  '(action location title) nil graph-overlay-proto)

(defmeth graph-control-proto :location (&optional (new nil set))
  (when set
        (send self :erase)
        (setf (slot-value 'location) new)
        (send self :redraw))
  (slot-value 'location))

(defmeth graph-control-proto :title (&optional (new nil set))
  (when set
        (send self :erase)
        (setf (slot-value 'title) new)
        (send self :redraw))
  (slot-value 'title))

(defmeth graph-control-proto :erase ()
  (let ((graph (send self :graph))
        (loc (send self :location))
        (sz (send self :size)))
    (if graph (apply #'send graph :erase-rect (append loc sz)))))

(defmeth graph-control-proto :size () 
  (let ((graph (send self :graph))
        (title (send self :title)))
    (if graph
        (list (+ 10 5 (send graph :text-width title)) 20)
        (list 10 10))))

(defmeth graph-control-proto :redraw ()
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (loc-x (first loc))
         (loc-y (second loc))
         (title (send self :title)))
    (send self :erase)
    (send graph :frame-rect loc-x (+ 5 loc-y) 10 10)
    (send graph :draw-text title (+ 15 loc-x) (+ 15 loc-y) 0 0)))

(defmeth graph-control-proto :do-click (x y a b)
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (loc-x (first loc))
         (loc-y (+ 5 (second loc))))
    (when (and (< loc-x x (+ loc-x 10)) (< loc-y y (+ loc-y 10)))
          (send graph :paint-rect (+ 1 loc-x) (+ 1 loc-y) 8 8)
          (send self :do-action (list a b))
          (send graph :while-button-down
                #'(lambda (x y) (send self :do-action nil)) nil)
          (send graph :erase-rect (+ 1 loc-x) (+ 1 loc-y) 8 8)
          t)))

(defmeth graph-control-proto :do-action (x) )

;;; Rockers
;;; written by Luke Tierney

(defproto rocker-control-proto () () graph-control-proto)

(defmeth rocker-control-proto :size () 
  (let ((graph (send self :graph))
        (title (send self :title)))
    (if graph
        (list (+ 10 5 10 5 (send graph :text-width title)) 20)
        (list 10 10))))

(defmeth rocker-control-proto :redraw ()
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (loc-x (first loc))
         (loc-y (second loc))
         (title (send self :title)))
    (send self :erase)
    (send graph :frame-rect loc-x (+ 5 loc-y) 10 10)
    (send graph :frame-rect (+ 15 loc-x) (+ 5 loc-y) 10 10)
    (send graph :draw-text title (+ 30 loc-x) (+ 15 loc-y) 0 0)))

(defmeth rocker-control-proto :do-click (x y a b)
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (loc-x1 (first loc))
         (loc-x2 (+ 15 loc-x1))
         (loc-y (+ 5 (second loc))))
    (if (< loc-y y (+ loc-y 10))
        (let* ((arg (cond 
                     ((< loc-x1 x (+ loc-x1 10)) '-)
                     ((< loc-x2 x (+ loc-x2 10)) '+)))
               (loc-x (case arg (- loc-x1) (+ loc-x2))))
          (when arg
                (send graph :paint-rect (+ 1 loc-x) (+ 1 loc-y) 8 8)
                (send self :do-action (list a b) arg)
                (send graph :while-button-down
                      #'(lambda (x y) (send self :do-action nil arg)) nil)
                (send graph :erase-rect (+ 1 loc-x) (+ 1 loc-y) 8 8)
                t)))))

(defmeth rocker-control-proto :do-action (x arg) (sysbeep))

;;; Sliders
;;; written by Luke Tierney

(defproto slider-control-proto 
  '(index sequence display) () graph-control-proto)

(defmeth slider-control-proto :isnew (sequence &key 
                                               (title "Value")
                                               (length 100)
                                               (display sequence)
                                               (location '(10 20))
                                               (index 0)
                                               graph) 
  (call-next-method :title title :location location)
  (send self :sequence sequence :display display)
  (send self :add-slot 'length length) 
  (send self :index index)
  (if graph (send graph :add-control self)))

(defmeth slider-control-proto :size () 
  (list (slot-value 'length) 30))

(defmeth slider-control-proto :redraw ()
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (loc-x (first loc))
         (loc-y (second loc))
         (w (first (send self :size))))
    (when graph
          (send graph :draw-text (send self :title) loc-x (+ loc-y 15) 0 0)
          (send graph :frame-rect loc-x (+ loc-y 20) w 10)
          (send self :draw-indicator))))

(defmeth slider-control-proto :draw-indicator (&optional index)
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (loc-x (first loc))
         (loc-y (second loc))
         (w (first (send self :size)))
         (min (send self :min))
         (max (send self :max))
         (index (if index index (send self :index)))
         (val (floor (* (- w 7) (/ (- index min) (- max min))))))
    (when graph
          (let ((tw (send graph :text-width (send self :title))))
            (send graph :start-buffering)
            (send graph :erase-rect (+ 1 tw loc-x) loc-y (- w tw) 20)
            (send graph :draw-text 
                  (format nil "~a" (elt (send self :display) index))
                  (+ loc-x w) (+ loc-y 15) 2 0)
            (send graph :buffer-to-screen (+ 1 tw loc-x) loc-y (- w tw) 20))
          (send graph :erase-rect (+ 1 loc-x) (+ 21 loc-y) (- w 2) 8)
          (send graph :paint-rect (+ 1 loc-x val) (+ 21 loc-y) 5 8))))

(defmeth slider-control-proto :min () 0)

(defmeth slider-control-proto :max () (- (length (slot-value 'sequence)) 1))

(defmeth slider-control-proto :sequence (&optional (seq nil set) &key 
                                                   (display seq))
  (when set
        (setf (slot-value 'sequence) (coerce seq 'vector))
        (setf (slot-value 'display) (coerce display 'vector)))
  (slot-value 'sequence))

(defmeth slider-control-proto :display () (slot-value 'display))

(defmeth slider-control-proto :index (&optional (new nil set))
  (if set
      (let* ((new (max (send self :min) (min new (send self :max))))) 
        (setf (slot-value 'index) new)
        (send self :draw-indicator)
        (send self :do-action (elt (send self :sequence) new))))
  (slot-value 'index))

(defmeth slider-control-proto :update-index (&optional (new nil set))
  (if set
      (let* ((new (max (send self :min) (min new (send self :max))))) 
        (setf (slot-value 'index) new)
        (send self :draw-indicator)))
  (slot-value 'index))

(defmeth slider-control-proto :do-click (x y a b)
;;;kludge to allow indicator to update when drawing next frame
  (when (and (null (send self :has-slot 'first-time))
             (send self :graph))
        (let ((control self))
          (send (send self :graph) :finish-next-frame 
                #'(lambda () (send control :redraw)))
          (send self :add-slot 'first-time nil)))
;;;end kludge
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (loc-x (nth 0 loc))
         (loc-y (nth 1 loc))
         (w (first (send self :size))))
    (when (and (< loc-x x (+ loc-x w)) (< (+ loc-y 20) y (+ loc-y 30)))
          (cond ((and a b) (send self :option-shift-click))
                (b (send self :option-click))
                (a (send self :shift-click))
                (t
          (let ((pos (+ (floor (* (- w 7) (/ (send self :index) 
                                             (send self :max))))
                        loc-x)))
            (cond
              ((<= pos x (+ pos 5))
               (let ((off (- x pos)))
                 (send graph :while-button-down
                       #'(lambda (x y)
                           (let ((val (max (+ loc-x 1)
                                           (min (- x off) 
                                                (+ loc-x (- w 6))))))
                             (setf pos val)
                             (send self :draw-indicator 
                                   (floor (* (send self :max) 
                                             (/ (- pos loc-x) (- w 7)))))))))
                 (send self :index 
                       (floor (* (send self :max) 
                                 (/ (- pos loc-x) (- w 7))))))
              ((< loc-x x pos)
               (send graph :while-button-down
                     #'(lambda (x y)
                         (let ((pos (+ (floor (* w (/ (send self :index) 
                                                      (send self :max))))
                                       loc-x)))
                           (if (< x pos)
                               (send self :index (- (send self :index) 1)))))
                     nil))
              ((< pos x (+ loc-x w))
               (send graph :while-button-down
                     #'(lambda (x y)
                         (let ((pos (+ (floor (* w (/ (send self :index) 
                                                      (send self :max))))
                                       loc-x)))
                           (if (> x pos)
                               (send self :index (+ (send self :index) 1)))))
                     nil))))))
          t)))

;;;;
;;;; Rotation example
;;; written by Luke Tierney
;;;;

;;; Rotation around axes

(defproto spin-rotate-control-proto '(v) () rocker-control-proto)

(defmeth spin-rotate-control-proto :isnew (v)
  (call-next-method :v v :location (list 10 (case v (0 10) (1 30) (2 50)))))

(defmeth spin-rotate-control-proto :title ()
  (send (send self :graph) :variable-label (slot-value 'v)))

(defmeth spin-rotate-control-proto :do-action (first sign)
  (let ((graph (send self :graph)))
    (if first
        (let* ((v (slot-value 'v))
               (v1 (if (= v 0) 1 0))
               (v2 (if (= v 2) 1 2))
               (trans (send graph :transformation))
               (cols (column-list 
                      (if trans 
                          trans 
                          (identity-matrix (send graph :num-variables)))))
               (angle (send graph :angle)))
          (send graph :idle-on (car first))
          (send graph :slot-value 'rotation-type
                (make-rotation (nth v1 cols) (nth v2 cols) 
                               (case sign (+ angle) (- (- angle)))))))
    (send graph :rotate)))

;;; Plot Rocking Control

(defproto spin-rock-control-proto '(v) () graph-control-proto)

(defmeth spin-rock-control-proto :isnew (loc)
  (call-next-method :location loc :title "Rock Plot"))

(defmeth spin-rock-control-proto :do-action (first) 
  (send (send self :graph) :rock-plot))

(defmeth spin-proto :rock-plot (&optional (a .15))
  (let* ((angle (send self :angle))
         (k (round (/ a angle))))
    (dotimes (i k) (send self :rotate-2 0 2 angle))
    (dotimes (i (* 2 k)) (send self :rotate-2 0 2 (- angle)))
    (dotimes (i k) (send self :rotate-2 0 2 angle))))

;;;;
;;;;
;;;; Symbol Button Overlay Proto
;;;; Written by James Harner, Univ. of West Virginia, used by permission
;;;;

(defproto symbol-button-overlay-proto '() nil graph-control-proto)

(defmeth symbol-button-overlay-proto :isnew (location)
  (call-next-method :location location))

;;;
;;; Other Methods
;;;

(defmeth symbol-button-overlay-proto :which-symbol (x y) 
  (let* ((s-x (+ 0 (first (send self :location))))
         (s-y (+ 10 (second (send self :location))))
         (y1 (- s-y 8))
         (y2 (- s-y 1))
         (y3 (+ s-y 1))
         (y4 (+ s-y 8)))
    (cond
      ((and (< (+ s-x  0) x (+ s-x  7)) (< y1 y y2)) 'dot)
      ((and (< (+ s-x  8) x (+ s-x 14)) (< y1 y y2)) 'dot1)
      ((and (< (+ s-x 15) x (+ s-x 22)) (< y1 y y2)) 'dot2)
      ((and (< (+ s-x 23) x (+ s-x 29)) (< y1 y y2)) 'dot3)
      ((and (< (+ s-x 30) x (+ s-x 37)) (< y1 y y2)) 'dot4)
      ((and (< (+ s-x 38) x (+ s-x 44)) (< y1 y y2)) 'disk)
      ((and (< (+ s-x  0) x (+ s-x  7)) (< y3 y y4)) 'diamond)
      ((and (< (+ s-x  8) x (+ s-x 14)) (< y3 y y4)) 'cross)
      ((and (< (+ s-x 15) x (+ s-x 22)) (< y3 y y4)) 'square)
      ((and (< (+ s-x 23) x (+ s-x 29)) (< y3 y y4)) 'wedge1)
      ((and (< (+ s-x 30) x (+ s-x 37)) (< y3 y y4)) 'wedge2)
      ((and (< (+ s-x 38) x (+ s-x 44)) (< y3 y y4)) 'x))))
      
(defmeth symbol-button-overlay-proto :redraw () 
  (let* ((s-x (+ 0 (first (send self :location))))
         (s-y (+ 10 (second (send self :location))))
         (plot (send self :graph)))
    (send plot :frame-rect s-x (- s-y 8) 45 17)
    (send plot :draw-symbol 'dot nil     (+ s-x  3) (- s-y 4))
    (send plot :draw-symbol 'dot1 nil    (+ s-x 10) (- s-y 4))
    (send plot :draw-symbol 'dot2 nil    (+ s-x 17) (- s-y 4))
    (send plot :draw-symbol 'dot3 nil    (+ s-x 24) (- s-y 4))
    (send plot :draw-symbol 'dot4 nil    (+ s-x 31) (- s-y 4))
    (send plot :draw-symbol 'disk nil    (+ s-x 40) (- s-y 4))
    (send plot :draw-symbol 'diamond nil (+ s-x  5) (+ s-y 4))
    (send plot :draw-symbol 'cross nil   (+ s-x 12) (+ s-y 4))
    (send plot :draw-symbol 'square nil  (+ s-x 18) (+ s-y 4))
    (send plot :draw-symbol 'wedge1 nil  (+ s-x 26) (+ s-y 4))
    (send plot :draw-symbol 'wedge2 nil  (+ s-x 33) (+ s-y 4))
    (send plot :draw-symbol 'x nil       (+ s-x 40) (+ s-y 4))))

(defmeth symbol-button-overlay-proto :do-click (x y m1 m2)
  (let* ((graph (send self :graph))
         (which (send graph :points-selected))
         (symbol (send self :which-symbol  x y)))
    (when (and symbol which)
          (dolist (plot (remove-duplicates (cons graph (send graph :links))))
                  (send plot :depth-cuing nil)
                  (send plot :point-symbol which symbol)
                  (send plot :redraw-content)
                  (send plot :points-selected which))
			 t)))

;;;;
;;;;
;;;; Color Button Overlay Proto
;;;; Written by James Harner, Univ. of West Virginia, used by permission
;;;;

(defproto color-button-overlay-proto '() nil graph-control-proto)

(defmeth color-button-overlay-proto :isnew (location)
  (call-next-method :location location))

;;;
;;; Other Methods
;;;

(defmeth color-button-overlay-proto :which-color (x y) 
  (let* ((s-x (+ 1  (first (send self :location))))
         (s-y (+ 10 (second (send self :location))))
         (y1 (- s-y 8))
         (y2 (- s-y 1))
         (y3 (+ s-y 1))
         (y4 (+ s-y 8)))
    (cond
      ((and (< (+ s-x  0) x (+ s-x  7)) (< y1 y y2)) (select *colors* 0))
      ((and (< (+ s-x  8) x (+ s-x 15)) (< y1 y y2)) (select *colors* 1))
      ((and (< (+ s-x 16) x (+ s-x 23)) (< y1 y y2)) (select *colors* 2))
      ((and (< (+ s-x 24) x (+ s-x 31)) (< y1 y y2)) (select *colors* 3))
      ((and (< (+ s-x  0) x (+ s-x  7)) (< y3 y y4)) (select *colors* 4))
      ((and (< (+ s-x  8) x (+ s-x 15)) (< y3 y y4)) (select *colors* 5))
      ((and (< (+ s-x 16) x (+ s-x 23)) (< y3 y y4)) (select *colors* 6))
      ((and (< (+ s-x 24) x (+ s-x 31)) (< y3 y y4)) (select *colors* 7)))))


(defmeth color-button-overlay-proto :redraw ()
  (let* ((s-x (+ 1 (first (send self :location))))
         (s-y (+ 10 (second (send self :location))))
         (plot (send self :graph))
         (color (send plot :draw-color)))
    (send plot :frame-rect (- s-x 1) (- s-y 8) 33 17)
    (send plot :draw-color (select *colors* 0))
    (send plot :paint-rect (+ s-x  0) (- s-y 7) 7 7)
    (send plot :draw-color (select *colors* 1))
    (send plot :paint-rect (+ s-x  8) (- s-y 7) 7 7)
    (send plot :draw-color (select *colors* 2))
    (send plot :paint-rect (+ s-x 16) (- s-y 7) 7 7)
    (send plot :draw-color (select *colors* 3))
    (send plot :paint-rect (+ s-x 24) (- s-y 7) 7 7)
    (send plot :draw-color (select *colors* 4))
    (send plot :paint-rect (+ s-x  0) (+ s-y 1) 7 7)
    (send plot :draw-color (select *colors* 5))
    (send plot :paint-rect (+ s-x  8) (+ s-y 1) 7 7)
    (send plot :draw-color (select *colors* 6))
    (send plot :paint-rect (+ s-x 16) (+ s-y 1) 7 7)
    (send plot :draw-color (select *colors* 7))
    (send plot :paint-rect (+ s-x 24) (+ s-y 1) 7 7)
    (send plot :draw-color color)))

(defmeth color-button-overlay-proto :do-click (x y m1 m2)
  (let* ((graph (send self :graph))
         (which (send graph :points-selected))
         (color (send self :which-color x y)))
    (when (and color which)
          (cond ((send graph :links)
                 (dolist (plot (send graph :links))
                         (send plot :point-color which color)
                         (send plot :redraw-content)
                         (send plot :points-selected which)))
            (t
             (send graph :point-color which color)
             (send graph :redraw-content)
             (send graph :points-selected which))))))

;;;
;;;  graph proto installation method for these protos
;;;

(defmeth graph-proto :install-color-symbol-buttons ()
  (let* ((loc (send self :locate-next-control :height 20 :absolute t))
         (loc2 (if (screen-has-color) (+ loc '(40 0)) loc)))
    (when (screen-has-color)
          (send self :use-color t)
          (send self :add-control (send color-button-overlay-proto :new loc)))
    (send self :add-control (send symbol-button-overlay-proto :new loc2))))

;;; checkbox control proto --- written by S. Weisberg

(defproto checkbox-control-proto '(selected) () graph-control-proto)

(defmeth checkbox-control-proto :bitmap (selected)
  (let* ((check-bitmap '#2a(( 1 0 0 0 0 0 0 1  )
                            ( 0 1 0 0 0 0 1 0  )
                            ( 0 0 1 0 0 1 0 0  )
                            ( 0 0 0 1 1 0 0 0  )
                            ( 0 0 0 1 1 0 0 0  )
                            ( 0 0 1 0 0 1 0 0  )
                            ( 0 1 0 0 0 0 1 0  )
                            ( 1 0 0 0 0 0 0 1  ) ))
         (blank-bitmap '#2a(( 0 0 0 0 0 0 0 0 )
                            ( 0 0 0 0 0 0 0 0 )
                            ( 0 0 0 0 0 0 0 0 )
                            ( 0 0 0 0 0 0 0 0 )
                            ( 0 0 0 0 0 0 0 0 )
                            ( 0 0 0 0 0 0 0 0 )
                            ( 0 0 0 0 0 0 0 0 )
                            ( 0 0 0 0 0 0 0 0 )))
         (loc (send self :location))
         (loc-x (first loc))
         (loc-y (second loc))
         (graph (send self :graph))
         (map (if selected check-bitmap blank-bitmap)))
    (send graph :draw-bitmap map (+ 1 loc-x) (+ 6 loc-y))))

(defmeth checkbox-control-proto :selected (&optional (new nil set))
  (when set (setf (slot-value 'selected) new))
  (slot-value 'selected))

(defmeth checkbox-control-proto :redraw () 
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (loc-x (first loc))
         (loc-y (second loc))
         (title (send self :title)))
    (send self :erase)
    (send graph :frame-rect loc-x (+ 5 loc-y) 10 10)
    (send graph :draw-text title (+ 15 loc-x) (+ 15 loc-y) 0 0)
    (send self :bitmap (slot-value 'selected))))

(defmeth checkbox-control-proto :do-click (x y a b)
  (let* ((out nil)
         (graph (send self :graph))
         (loc (send self :location))
         (loc-x (first loc))
         (loc-y (+ 5 (second loc))))
    (when (and (< loc-x x (+ loc-x 10)) (< loc-y y (+ loc-y 10)))
          (setf out t)
          (cond
            ((and a b) (send self :option-shift-click))
            (a (send self :shift-click))
            (b (send self :option-click))
            (t
             (send self :selected (not (send self :selected)))
             (send self :do-action ))))
    out))
(defmeth graph-control-proto :option-shift-click () (sysbeep))
(defmeth graph-control-proto :option-click () (sysbeep))
(defmeth graph-control-proto :shift-click () (sysbeep))
(defmeth checkbox-control-proto :do-action () (sysbeep))

;;;;
;;;; Instances of Checkbox-control-proto
;;;;


;;;
;;;  toggle-linear-trend-control-proto
;;;

(defproto toggle-linear-trend-control-proto '(data now-computing)
  () checkbox-control-proto)
(defmeth toggle-linear-trend-control-proto :isnew (loc title)
  (call-next-method :location loc :title title))
(defmeth toggle-linear-trend-control-proto :do-action ()
  (send (send self :graph) :add-slot 'detrended 
        (if (send self :selected) t nil))
  (setf (slot-value 'now-computing) t)
  (if (send self :selected)
      (send self :delete-trend)
      (send self :restore-trend))
  (setf (slot-value 'now-computing) nil))
(defmeth toggle-linear-trend-control-proto :delete-trend ()
  (let* ((graph (send self :graph))
         (n (iseq (send graph :num-points)))
         (inc (let* ((a (make-list (send graph :num-points))))
                    (setf (select a (send graph :points-showing))
                          (repeat t (length (send graph :points-showing))))
                    a))
         (p (iseq (send graph :num-point-variables)))
         (d (send graph :point-coordinate p (repeat (list n) (length p))))
         (r (regression-model (rmel 1 d) (select d 1) :print nil
                              :included inc)))
        (setf (slot-value 'data) (send r :y))
        (send graph :draw-next-frame '(1) (list (send r :raw-residuals)))))
(defmeth toggle-linear-trend-control-proto :restore-trend ()
  (let* ((data (slot-value 'data))
         (graph (send self :graph)))
        (send graph :draw-next-frame '(1) (list data))))

;;; installation method

(defmeth graph-proto :install-toggle-linear-trend ()
  (let* ((control (send toggle-linear-trend-control-proto :new 
                        (send self :locate-next-control) "Rem.Lin.Trend")))
        (send self :finish-next-frame #'(lambda ()
                (cond ((send control :slot-value 'now-computing) 
                       (send self :adjust-to-data))
                      ((send control :slot-value 'selected)
                       (send control :delete-trend)
                       (send self :adjust-to-data))
                      (t ))))
        (send self :add-control control)))

