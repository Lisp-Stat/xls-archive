;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The power-norm is the 1/p power of the sum of the absolute values 
;; to the p-th power. Define the power-loss of a number y for a vector
;; x_1,...,x_n to be the power-mean of the x_i - y. Define the power
;; mean of x_1,...,x_n to be the y minimizing the power loss.
;;
;; We all know the median is the power-mean for p = 1, the mean is the
;; power mean for p = 2, the mode is the power-mean for p -> 0, and
;; the mid-range is the power-mean for p -> infty.
;;
;; Here is a demo with a slider for p. You simply say (power-mean x)
;; for some x, select your power, and move the mouse in the window
;; with the point-plot.
;;
;; It flickers, but it works.
;;
;; Jan de Leeuw, 11-17-96
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto point-window-proto
  '(x power loss offset) nil graph-window-proto)

(defmeth point-window-proto :x (&optional (val nil set))
 (if set (setf (slot-value 'x) val))
  (slot-value 'x))

(defmeth point-window-proto :power (&optional (val nil set))
 (if set (setf (slot-value 'power) val))
  (slot-value 'power))

(defmeth point-window-proto :offset (&optional (val nil set))
 (if set (setf (slot-value 'offset) val))
  (slot-value 'offset))

(defmeth point-window-proto :loss (&optional (val nil set))
  (if set (setf (slot-value 'loss) val))
  (slot-value 'loss))

(defmeth point-window-proto :coord-map (x)
(let* (
      (o (send self :offset))
      (s (- (first (send self :size)) (+ o o 15)))
      (b (max (send self :x)))
      (a (min (send self :x)))
      (c (- b a))
      (r (/ s c))
      (e (- o (* r a)))
      (d (second (/ (send self :size) 2)))
      )
 (list (round (+ (* r (first x)) e))
       (round (* d (- 1 (second x)))))
))

(defmeth point-window-proto :a-line (x y)
  (let* (
        
        (u (send self :coord-map x))
        (v (send self :coord-map y))
        )
(send self :draw-line (first u) (second u) (first v) (second v))
))

(defmeth point-window-proto :redraw ()
  (send self :make-points)
)

(defmeth point-window-proto :make-points ()
(let* ((x (send self :x))
       (a (min x))
       (b (max x)))
  (send self :a-line (list a 0) (list b 0))
  (dolist (z x)
        (send self :a-line (list z -.5) (list z .5)))
))

(defmeth point-window-proto :do-motion (x y)
 (let ((s (second (send self :size))))
  (send self :erase-window)
  (send self :make-points)
  (send self :draw-line x 0 x s)
  (send self :function-line x)
))

(defmeth point-window-proto :function-line (x)
(let* (
      (z (send self :x))
      (o (send self :offset))
      (s (- (first (send self :size)) (+ o o 15)))
      (b (max z))
      (a (min z))
      (c (- b a))
      (r (/ s c))
      (e (- o (* r a)))
      (h (send self :loss))
      (u (/ (- x e) r))
      (v (send h :range 1))
      (power (send self :power))
      )
(send h :clear-lines)
(send h :add-function 
      (power-loss z power) a b)
(send h :add-lines (list u u) v)
(format t "Argument ~10,3g Value ~20,6g~%"
        u (funcall (power-loss z power) u))
))

(defun points-on-line (x power &key (size 300) (offset 10))
(let ((g (send point-window-proto :new 
               :size (list size (/ size 4)) 
               :title "Point Window"
               :has-h-scroll nil
               :has-v-scroll nil)))
(send g :x x)
(send g :offset offset)
(send g :power power)
(send g :loss 
      (plot-function (power-loss x power) (min x) (max x)))
(send g :make-points)
(send (send g :loss) :location 200 200)
))

(defun power-loss (x power)
  (lambda (y) (expt (sum (expt (abs (- x y)) power)) (/ power)))
)

(defun range (x)
(- (max x) (min x)))

(defun power-mean (x)
  (sequence-slider-dialog
   '(.01 .1 .5 1 1.5 2 3 4 5 10 100)
    :action #'(lambda (p) (points-on-line x p)))
  )

