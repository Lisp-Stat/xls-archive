(setf x (append (iseq 1 18) (list 30 40)))
(setf y (+ x (* 2 (normal-rand 20))))

(setf p (plot-points x y))

(send p :add-mouse-mode 'point-moving
      :title "Point Moving"
      :cursor 'finger
      :click :do-point-moving)

(defmeth p :do-point-moving (x y a b)
  (let ((p (send self :drag-point x y :draw nil)))
    (if p (send self :set-regression-line))))

(defmeth p :set-regression-line ()
  (let ((coefs (send self :calculate-coefs)))
    (send self :clear-lines :draw nil)
    (send self :abline (select coefs 0) (select coefs 1))))

(defmeth p :calculate-coefs ()
  (let* ((i (iseq (send self :num-points)))
         (x (send self :point-coordinate 0 i))
         (y (send self :point-coordinate 1 i))
         (m (regression-model x y :print nil)))
    (send m :coef-estimates)))

(send p :set-regression-line)

(send p :mouse-mode 'point-moving)
