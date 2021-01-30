;; Here is the basic prototype we use. It inherits from the
;; graph-window-proto, and introduces six new slots

(defproto power-window-proto
  '(a c s x done eps) nil
  graph-window-proto)

;; Assessor methods for the five slots. These either
;; display the value in the slot, if called without an
;; argument, or they put an object in a slot if called
;; with an argument

(defmeth power-window-proto :a (&optional (val nil set))
 (if set (setf (slot-value 'a) val))
  (slot-value 'a))

(defmeth power-window-proto :c (&optional (val nil set))
 (if set (setf (slot-value 'c) val))
  (slot-value 'c))

(defmeth power-window-proto :s (&optional (val nil set))
 (if set (setf (slot-value 's) val))
  (slot-value 's))

(defmeth power-window-proto :x (&optional (val nil set))
 (if set (setf (slot-value 'x) val))
  (slot-value 'x))

(defmeth power-window-proto :done (&optional (val nil set))
 (if set (setf (slot-value 'done) val))
  (slot-value 'done))

(defmeth power-window-proto :eps (&optional (val nil set))
 (if set (setf (slot-value 'eps) val))
  (slot-value 'eps))

;;
;; Some methods that provide some assistance
;;

(defmeth power-window-proto :coord-map (x)
(let* (
      (s (send self :s))
      (c (send self :c))
      (b (/ s c))
      )
 (list (round (+ s (* b (first x)))) (round (- s (* b (second x)))))
))

(defmeth power-window-proto :a-line (x y)
  (let (
        (u (send self :coord-map x))
        (v (send self :coord-map y))
        )
(send self :draw-line (first u) (second u) (first v) (second v))
))

(defmeth power-window-proto :e-line (x y)
  (let (
        (u (send self :coord-map x))
        (v (send self :coord-map y))
        )
(send self :draw-mode 'xor)
(send self :draw-line (first u) (second u) (first v) (second v))
(send self :draw-mode 'normal)
))

(defmeth power-window-proto :unit-circle ()
(let (
      (u (send self :coord-map '(-1 +1)))
      (b (* 2 (/ (send self :s) (send self :c))))
      )
 (send self :frame-oval (first u) (second u) b b)
))

(defmeth power-window-proto :redraw ()
  (send self :unit-circle)
  (send self :a-line '(0 0) (send self :x))
)

(defmeth power-window-proto :an-iter ()
  (let* (
        (old-x (send self :x))
        (raw-x (update (send self :a) old-x))
        (new-x (normalize raw-x))
        (pnt-x (send self :coord-map new-x))
        )
(send self :erase-window)
(send self :unit-circle)
(pause 20)
(send self :a-line '(0 0) old-x)
(pause 20)
(send self :a-line '(0 0) raw-x)
(pause 20)
(send self :e-line '(0 0) raw-x)
(send self :a-line '(0 0) new-x)
(send self :draw-string (format nil "~8,4f" (elength raw-x))
      (first pnt-x) (second pnt-x))
(pause 40)
(if (test old-x new-x (send self :eps)) 
    (send self :done t)
    (send self :x new-x))
))

;; and the constructor function

(defun power-method (a &key (size 300) (c 3) (eps 1e-6))
(let (
     (w (send power-window-proto :new :size (repeat size 2) 
               :title "Power Method"))
     )
(send w :s (/ size 2))
(send w :c c)
(send w :a a)
(send w :x '(1 0))
(send w :eps eps)
(send w :done nil)
(loop (if (send w :done) (return) (send w :an-iter)))
))

;; These are some simple helpers

(defun update (a x)
  (matmult a x)
)

(defun normalize (x)
  (/ x (elength x))
)

(defun test (x y eps)
  (> eps (max (abs (- x y))))
)

(defun elength (x)
  (sqrt (sum (* x x)))
)

;; And fire away ...

;;(power-method #2a((2 -1)(-1 2)))
(power-method #2a((1 .01)(.01 2)))
;;(power-method #2a((2 1)(-2 1)))
