;;;
;;;  basic plot control prototypes
;;;
;;; graph control proto
;;; written by Luke Tierney, similar to the button-overlay-proto on
;;; page 313 of Tierney (1990).


(defmeth graph-proto :slider-width ()
  (+ 15 (round (* *relative-slidebar-length* 
                  (send self :text-width "Rem. Lin. Trend")))))
    
(defmeth graph-proto :add-control (c) (send self :add-overlay c))

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
    (send graph :draw-text title (+ 15 loc-x) (+ 14 *print-offset* loc-y) 0 0)))

(defmeth graph-control-proto :do-click (x y a b)
  (let* ((graph (send self :graph))
         (size (send self :size))
         (loc (send self :location))
         (loc-x (first loc))
         (loc-y (+ 5 (second loc))))
    (when (and (< loc-x x (+ loc-x (first size))) 
               (< loc-y y (+ loc-y (second size))))
          (send graph :paint-rect (+ 1 loc-x) (+ 1 loc-y) 8 8)
          (send self :do-action (list a b))
          (send graph :while-button-down
                #'(lambda (x y) (send self :do-action nil)) nil)
          (send graph :erase-rect (+ 1 loc-x) (+ 1 loc-y) 8 8)
          t)))

(defmeth graph-control-proto :do-action (x) )

;;;
;;;      Slider-control-proto written by Luke Tierney
;;;

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
  (if graph (send graph :add-overlay self)))

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
	  (if (boundp '*total-count*)
	      (send self :draw-indicator 
		    (floor (* 1000 (/ *success-count* *total-count*))))
	    (send self :draw-indicator)))))

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
                  (f-fmt (elt (send self :display) index))
                  (+ loc-x w) (+ loc-y 15) 2 0)
            (send graph :buffer-to-screen (+ 1 tw loc-x) loc-y (- w tw) 20))
          (send graph :erase-rect (+ 1 loc-x) (+ 21 loc-y) (- w 2) 8)
          (send graph :paint-rect (+ 1 loc-x) (+ 21 loc-y) (1+ val) 8))))
;;          (send graph :paint-rect (+ 1 loc-x val) (+ 21 loc-y) 5 8))))

;(defun f-fmt (num) (format nil "~a" num))
(defun f-fmt (num) (format nil "~5,3f" num))
;  For use with compiled version, replace the last line with:
;(defun f-fmt (num)
;    (cond ((null num) "NIL") ((integerp num) (format nil "~a" num))
;          ((stringp num) num) (t (format nil "~6f" num))))


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
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (loc-x (nth 0 loc))
         (loc-y (nth 1 loc))
         (w (first (send self :size))))
    (cond ((and (< loc-x x (+ loc-x w)) (< (+ loc-y 3) y (+ loc-y 20))
                *shift-click-on-title*)
           (send self :shift-click) t)
    ((and (< loc-x x (+ loc-x w)) (< (+ loc-y 20) y (+ loc-y 30)))
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
          t))))

