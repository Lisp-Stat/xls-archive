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
