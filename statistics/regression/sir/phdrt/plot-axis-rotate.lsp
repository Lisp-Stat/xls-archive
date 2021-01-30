(defproto button-overlay-proto
  '(location title)
  nil
  graph-overlay-proto)

(defmeth button-overlay-proto :location (&optional new)
  (if new (setf (slot-value 'location) new))
  (slot-value 'location))

(defmeth button-overlay-proto :title (&optional new)
  (if new (setf (slot-value 'title) new))
  (slot-value 'title))

(send button-overlay-proto :location '(0 0))

(send button-overlay-proto :title "Button")

(defmeth button-overlay-proto :size ()
  (let* ((graph (send self :graph))
         (title (send self :title))
         (text-width (send graph :text-width title))
         (side (send graph :text-ascent))
         (gap (floor (/ side 2)))
         (descent (send graph :text-descent))
         (height (+ side descent (* 2 gap))))
    (list (+ side (* 3 gap) text-width) height)))

(defmeth button-overlay-proto :button-box ()
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (side (send graph :text-ascent))
         (gap (floor (/ side 2))))
    (list (+ gap (first loc)) (+ gap (second loc)) side side)))

(defmeth button-overlay-proto :title-start ()
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (title (send self :title))
         (side (send graph :text-ascent))
         (gap (floor (/ side 2))))
    (list (+ (* 2 gap) side (first loc))
          (+ gap side (second loc)))))

(defmeth button-overlay-proto :draw-button (&optional paint)
  (let ((box (send self :button-box))
        (graph (send self :graph)))
    (apply #'send graph :erase-rect box)
    (if paint
        (apply #'send graph :paint-rect box)
        (apply #'send graph :frame-rect box))))

(defmeth button-overlay-proto :draw-title ()
  (let ((graph (send self :graph))
        (title (send self :title))
        (title-xy (send self :title-start)))
    (apply #'send graph :draw-string title title-xy)))

(defmeth button-overlay-proto :redraw ()
  (send self :draw-title)
  (send self :draw-button))

(defmeth button-overlay-proto :point-in-button (x y)
  (let* ((box (send self :button-box))
         (left (first box))
         (top (second box))
         (side (third box)))
    (and (< left x (+ left side)) (< top y (+ top side)))))

(defmeth button-overlay-proto :do-click (x y m1 m2)
  (let ((graph (send self :graph)))
    (when (send self :point-in-button x y)
          (send self :draw-button t)
          (send self :do-action (list m1 m2))
          (send graph :while-button-down
                #'(lambda (x y) (send self :do-action nil))
                nil)
          (send self :draw-button nil)
          t)))

(defmeth button-overlay-proto :do-action (x) nil)

(defmeth spin-proto :rock-plot (&optional (a .15))
  (let* ((angle (send self :angle))
         (k (round (/ a angle))))
    (dotimes (i k) (send self :rotate-2 0 2 angle))
    (dotimes (i (* 2 k)) (send self :rotate-2 0 2 (- angle)))
    (dotimes (i k) (send self :rotate-2 0 2 angle))))

(defproto spin-rock-control-proto () () button-overlay-proto)

(send spin-rock-control-proto :title "Rock Plot")

(defmeth spin-rock-control-proto :do-action (first)
  (send (send self :graph) :rock-plot))

(defmeth spin-rock-control-proto :resize ()
  (let* ((graph (send self :graph))
         (size (send self :size))
         (width (send self :canvas-width))
         (height (send graph :canvas-height)))
    (send self :location (- (list width (+ 3 height)) size))))

(defproto twobutton-control-proto () () button-overlay-proto)

(defmeth twobutton-control-proto :size ()
  (let* ((graph (send self :graph))
         (size (call-next-method))
         (side (send graph :text-ascent))
         (gap (floor (/ side 2))))
    (list (+ gap side (first size)) (second size))))

(defmeth twobutton-control-proto :title-start ()
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (title (send self :title))
         (side (send graph :text-ascent))
         (gap (floor (/ side 2))))
    (list (+ (* 3 gap) (* 2 side) (first loc))
          (+ gap side (second loc)))))

(defmeth twobutton-control-proto :button-box (which)
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (side (send graph :text-ascent))
         (gap (floor (/ side 2)))
         (left (case which
                 (+ (+ gap (first loc)))
                 (- (+ (* 2 gap) side (first loc))))))
    (list left (+ gap (second loc)) side side)))

(defmeth twobutton-control-proto :draw-button (which &optional paint)
  (let ((box (send self :button-box which))
        (graph (send self :graph)))
    (cond (paint (apply #'send graph :paint-rect box))
      (t (apply #'send graph :erase-rect box)
         (apply #'send graph :frame-rect box)))))

(defmeth twobutton-control-proto :redraw ()
  (send self :draw-title)
  (send self :draw-button '-)
  (send self :draw-button '+))

(defmeth twobutton-control-proto :point-in-button (x y)
  (let* ((box1 (send self :button-box '-))
         (box2 (send self :button-box '+))
         (left1 (first box1))
         (top (second box1))
         (side (third box1))
         (left2 (first box2)))
    (cond
      ((and (< left1 x (+ left1 side)) (< top y (+ top side)))
       '-)
      ((and (< left2 x (+ left1 side)) (< top y (+ top side)))
       '+))))

(defmeth twobutton-control-proto :do-click (x y m1 m2)
  (let ((graph (send self :graph))
        (which (send self :point-in-button x y)))
    (when which
          (send self :draw-button which t)
          (send self :do-action which (list m1 m2))
          (send graph :while-button-down
                #'(lambda (x y) 
                    (send self :do-action which nil))
                nil)
          (send self :draw-button which nil)
          t)))

(defmeth twobutton-control-proto :do-action (which mods) nil)

(defmeth spin-proto :set-axis-rotation (v)
  (let* ((m (send self :num-variables))
         (v1 (if (= v 0) 1 0))
         (v2 (if (= v 2) 1 2))
         (trans (send self :transformation))
         (cols (column-list
                (if trans trans (identity-matrix m))))
         (x1 (select cols v1))
         (x2 (select cols v2))
         (angle (send self :angle)))
    (send self :rotation-type (make-rotation x1 x2 angle))))

(defproto spin-rotate-control-proto
  '(v) () twobutton-control-proto)

(defmeth spin-rotate-control-proto :isnew (v &rest args)
  (apply #'call-next-method :v v args))

(defmeth spin-rotate-control-proto :title ()
  (send (send self :graph) :variable-label (slot-value 'v)))

(defmeth spin-rotate-control-proto :do-action (sign mods)
  (let ((graph (send self :graph)))
    (if mods
        (let ((v (slot-value 'v))
              (angle (abs (send graph :angle))))
          (send graph :idle-on (first mods))
          (send graph :angle
                (if (eq sign '+) angle (- angle)))
          (send graph :set-axis-rotation v)))
    (send graph :rotate)))

(defun width1 (c) (first (send c :size)))
  
(defmeth spin-proto :axis-rotate ()
    (let* ((c0 (send spin-rotate-control-proto :new 0))
           (c1 (send spin-rotate-control-proto :new 1))
           (c2 (send spin-rotate-control-proto :new 2)))
      (send self :add-overlay c0)
      (send self :add-overlay c1)
      (send self :add-overlay c2)
      (let ((width (max (list (width1 c0) (width1 c1) (width1 c2))))
            (height (second (send c0 :size)))
            (margin (send self :margin)))
        (send c1 :location (list 0 height))
        (send c2 :location (list 0 (* 2 height)))
        (send self :margin width 0 0 (fourth margin)))))

(send spin-proto :add-mouse-mode 'hand-rotate
      :title "Hand Rotate"
      :cursor 'hand
      :click :do-hand-rotate)

(defmeth spin-proto :canvas-to-sphere (x y rad)
  (let* ((p (send self :canvas-to-scaled x y))
         (x (first p))
         (y (second p))
         (norm-2 (+ (* x x) (* y y)))
         (rad-2 (^ rad 2))
         (z (sqrt (max (- rad-2 norm-2) 0))))
    (if (< norm-2 rad-2)
        (list x y z)
        (let ((r (sqrt (/ norm-2 rad-2))))
          (list (/ x r) (/ y r) (/ z r))))))

(defmeth spin-proto :do-hand-rotate (x y m1 m2)
  (let* ((m (send self :num-variables))
         (range (send self :scaled-range 0))
         (rad (/ (apply #'- range) 2))
         (oldp (send self :canvas-to-sphere x y rad))
         (p oldp)
         (vars (send self :content-variables))
         (trans (identity-matrix m)))
    (flet ((spin-sphere (x y)
                        (setf oldp p)
                        (setf p (send self :canvas-to-sphere x y rad))
                        (setf (select trans vars vars)
                              (make-rotation oldp p))
                        (when m1
                              (send self :rotation-type trans)
                              (send self :idle-on t))
                        (send self :apply-transformation trans)))
      (send self :idle-on nil)
      (send self :while-button-down #'spin-sphere))))
