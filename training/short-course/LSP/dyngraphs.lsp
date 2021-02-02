(new-section "Dynamic" "dyngraphs.scr")

(load "abrasion")
(load "precip")
(load-data "stackloss")
(load "river")
(load "smallplaces")
(load "hip")
(load-data "diabetes")

(def times '(9 13 13 18 23 28 31 34 45 48 161
             5 5 8 8 12 16 23 27 30 33 43 45))
(def status '(1 1 0 1 1 0 1 1 0 1 0 1 1 1 1 1 0 1 1 1 1 1 1))

(define-section-item "River" (plot-points (iseq 1 128) river))

(define-section-item "Profile" 
  (let* ((resp (list occ1 occ2 occ4))
         (p (histogram age))
         (q (plot-lines (iseq 1 3) (mapcar #'mean resp))))
    (send q :range 0 0 4)
    (send q :range 1 (min resp) (max resp))
    (defmeth p :adjust-screen ()
      (call-next-method)
      (let ((i (union (send self :points-selected)
                      (send self :points-hilited))))
        (send q :clear-lines :draw nil)
        (if i
            (flet ((ms (x) (mean (select x i))))
              (let ((y (mapcar #'ms resp))
                    (j (iseq 1 (length resp))))
                (send q :add-lines j y)))
            (send q :redraw-content))))))

(define-section-item "Survival" 
  (let* ((n 100)
         (cp 0.1)
         (x (uniform-rand n))
         (y (uniform-rand n))
         (z (* x y))
         (times (/ (- (log (uniform-rand n))) (+ (* 10 x) y)))
         (status (binomial-rand n 1 (- 1 cp)))
         (ord (order times)))
    (flet ((sel-ord (v) (select v ord)))
      (let* ((x (sel-ord x))
             (y (sel-ord y))
             (z (sel-ord z))
             (times (sel-ord times))
             (status (sel-ord status))
             (p (scatterplot-matrix (list x y z)))
             (q (plot-lines (make-steps times (km times status)))))
        (defmeth p :adjust-screen ()
          (call-next-method)
          (let ((s (union (send self :points-selected)
                          (send self :points-hilited))))
            (send q :clear-lines :draw nil)
            (if s
                (let* ((s (sort-data s))
                       (tm (select times s))
                       (st (select status s)))
                  (send q :add-lines (make-steps tm (km tm st))))
                (send q :redraw-content))))))))

(define-section-item "Overlays"
  (let ((p (spin-plot (list hardness tensile-strength abrasion-loss))))
    (send p :add-spin-controls)))

(define-section-item "Interpolate"
  (interp-plot (list longitude latitude climate-terrain housing)
               :point-labels city-state))

(define-section-item "Grand Tour"
  (tour-plot diabetes :variable-labels dlabs))

(define-section-item "Normality..."
  (flet ((gamma (z)
           (let ((y (+ 10 (* (sqrt 10) z))))
             (if (< 0 (min y))
                 (prod (* (^ (/ y 10) 10) (exp (- 10 y))))
                 0)))
          (nmix (z)
                (let ((z1 (- z '(2 0 0))))
                  (+ (exp (* -0.5 (sum (* z z))))
                     (* 0.5 (exp (* -2 (sum (* z1 z1)))))))))
    (let ((gamma-d '(1 0 0 0))
          (nmix-d '(0 1 0)))
      (case (choose-item-dialog "Function" '("Gamma" "Normal Mix"))
        (0 (send ncheck-plot-proto :new #'gamma gamma-d))
        (1 (send ncheck-plot-proto :new #'nmix nmix-d))))))

(defun km (x s)
  (let ((n (length x)))
    (accumulate #'* (- 1 (/ s (iseq n 1))))))

(defun make-steps (x y)
  (let* ((n (length x))
         (i (iseq (+ (* 2 n) 1))))
    (list (append '(0) (repeat x (repeat 2 n)))
          (select (repeat (append '(1) y) (repeat 2 (+ n 1))) i))))

;; Data Set Prototypes
(defproto data-set-proto '(data title))

(send data-set-proto :slot-value 'title "a data set")

(defmeth data-set-proto :title (&optional (title nil set))
  (if set (setf (slot-value 'title) title))
  (slot-value 'title))

(defmeth data-set-proto :data (&optional (data nil set))
  (if set (setf (slot-value 'data) data))
  (slot-value 'data))

(defmeth data-set-proto :describe (&optional (stream t))
  (let ((title (send self :title))
        (data (send self :data)))
    (format stream "This is ~a~%" title)
    (format stream "The sample mean is ~a~%" (mean data))
    (format stream "The sample SD is ~a~%" (standard-deviation data))))

(defmeth data-set-proto :print (&optional (stream t))
  (format stream "#<~a>" (send self :title)))

(defmeth data-set-proto :plot ()
  (histogram (send self :data) :title (send self :title)))

(setf x (send data-set-proto :new :data (chisq-rand 20 5)))

(defmeth data-set-proto :isnew (data &key title)
  (send self :data data)
  (if title (send self :title title)))

;; Time Series Objects
(defproto time-series-proto '(origin spacing) () data-set-proto)

(defmeth time-series-proto :origin (&optional (origin nil set))
  (if set (setf (slot-value 'origin) origin))
  (slot-value 'origin))

(defmeth time-series-proto :spacing (&optional (sp nil set))
  (if set (setf (slot-value 'spacing) sp))
  (slot-value 'spacing))

(send time-series-proto :title "a time series")
(send time-series-proto :origin 0)
(send time-series-proto :spacing 1)

(let* ((e (normal-rand 21))
       (i (iseq 1 20))
       (d (+ (select e i) (* 0.6 (select e (- i 1))))))
  (setf y (send time-series-proto :new d)))

(defmeth time-series-proto :plot ()
  (let* ((data (send self :data))
         (start (send self :origin))
         (step (send self :spacing))
         (n (length data)))
    (plot-points (+ start (* step (iseq n))) data)))

(defmeth time-series-proto :describe (&optional (stream t))
  (let ((ac (autocor (send self :data))))
    (call-next-method stream)
    (format stream "The autocorrelation is ~a~%" ac)))

(defun autocor (x)
  (let ((n (length x))
        (x (- x (mean x))))
    (/ (mean (* (select x (iseq 0 (- n 2))) 
                (select x (iseq 1 (- n 1)))))
       (mean (* x x)))))

;; Survival Function Objects
(defproto survival-proto
          '(death-times num-deaths num-at-risk)
          ()
          data-set-proto)

(send survival-proto :title "a survival data set")

(defmeth survival-proto :death-times () (slot-value 'death-times))
(defmeth survival-proto :num-deaths () (slot-value 'num-deaths))
(defmeth survival-proto :num-at-risk () (slot-value 'num-at-risk))

(defmeth survival-proto :data (&optional times status)
  (when times
    (call-next-method (list times status))
    (let* ((i (which (= 1 status)))
           (dt (select times i))
           (dt-list (coerce dt 'list))
           (udt (sort-data (remove-duplicates dt-list :test #'=)))
           (d (mapcar #'(lambda (x) (count x dt-list :test #'=)) udt))
           (r (mapcar #'(lambda (x) (count x times :test #'<=)) udt)))
      (setf (slot-value 'death-times) udt)
      (setf (slot-value 'num-deaths) d)
      (setf (slot-value 'num-at-risk) r)))
  (slot-value 'data))

(defmeth survival-proto :isnew
         (times status &optional title)
  (send self :data times status)
  (if title (send self :title title)))

(defmeth survival-proto :describe (&optional (stream t))
  (let ((title (send self :title))
        (data (send self :data)))
    (format stream "This is ~a~%" title)
    (format stream "The mean time is ~a~%" (mean (first data)))
    (format stream "The number of failures is ~a~%" (sum (second data)))))

(defmeth survival-proto :plot ()
  (let ((km (send self :km-estimator))
        (udt (send self :death-times)))
    (plot-lines (make-steps udt km))))

(defmeth survival-proto :km-estimator ()
  (let ((r (send self :num-at-risk))
        (d (send self :num-deaths)))
    (accumulate #'* (/ (- r d) r))))

(defmeth survival-proto :fh-estimator ()
  (let ((r (send self :num-at-risk))
        (d (send self :num-deaths)))
    (exp (- (cumsum (/ d r))))))

(defmeth survival-proto :greenwood-se ()
  (let* ((r (send self :num-at-risk))
         (d (send self :num-deaths))
         (km (send self :km-estimator))
         (rd1 (pmax (- r d) 1)))
    (* km (sqrt (cumsum (/ d r rd1))))))

(defmeth survival-proto :tsiatis-se ()
  (let ((r (send self :num-at-risk))
        (d (send self :num-deaths))
        (km (send self :km-estimator)))
    (* km (sqrt (cumsum (/ d (^ r 2)))))))

(setf s (send survival-proto :new times status))

;; Plot Interpolation
(defun interp-plot (data &rest args)
  ;; Should check here that data is 4D
  (let ((w (apply #'plot-points data :scale 'variable args))
        (m (matrix '(4 4) (repeat 0 16))))
    (flet ((interpolate (p)
             (let* ((a (* (/ pi 2) p))
                    (s (sin a))
                    (c (cos a)))
               (setf (select m 0 0) c)
               (setf (select m 0 2) s)
               (setf (select m 1 1) c)
               (setf (select m 1 3) s)
               (send w :transformation m))))
      (let ((s (interval-slider-dialog '(0 1)
                                       :points 25
                                       :action #'interpolate)))
        (send w :add-subordinate s)))
    w))

(defproto run-item-proto '(graph) () menu-item-proto)

(send run-item-proto :key #\R)

(defmeth run-item-proto :isnew (graph)
  (call-next-method "Run")
  (setf (slot-value 'graph) graph))

(defmeth run-item-proto :update ()
  (send self :mark (send (slot-value 'graph) :idle-on)))

(defmeth run-item-proto :do-action ()
  (let ((graph (slot-value 'graph)))
    (send graph :idle-on (not (send graph :idle-on)))))

;; Grand Tour
(defun tour-plot (&rest args)
  (let ((p (apply #'spin-plot args)))
    (send p :add-slot 'tour-count -1)
    (send p :add-slot 'tour-trans nil)
    (defmeth p :tour-step ()
      (when (< (slot-value 'tour-count) 0)
        (let ((vars (send self :num-variables))
              (angle (send self :angle)))
          (setf (slot-value 'tour-count) (random 20))
          (setf (slot-value 'tour-trans) 
                (make-rotation (sphere-rand vars) (sphere-rand vars) angle))))
      (send self :apply-transformation (slot-value 'tour-trans))
      (setf (slot-value 'tour-count) (- (slot-value 'tour-count) 1)))
    (defmeth p :do-idle () (send self :tour-step))
    (send (send p :menu) :append-items (send run-item-proto :new p))
    p))

(defun sphere-rand (n)
  (let* ((z (normal-rand n))
         (r (sqrt (sum (^ z 2)))))
    (if (< 0 r) (/ z r) (repeat (/ (sqrt n)) n))))
;**** examples

;; Normality Check Tour
(defproto ncheck-plot-proto
          '(function direction xvals tour-count tour-trans angle)
          ()
          scatterplot-proto)

(send ncheck-plot-proto :slot-value 'tour-count -1)
(send ncheck-plot-proto :slot-value 'angle 0.2)

(defmeth ncheck-plot-proto :set-image ()
  (let* ((x (slot-value 'xvals))
         (f (slot-value 'function))
         (d (slot-value 'direction))
         (y (mapcar #'(lambda (x) (funcall f (* x d))) x)))
    (send self :clear-lines :draw nil)
    (send self :add-lines (spline x y))))

(defmeth ncheck-plot-proto :tour-step ()
  (when (< (slot-value 'tour-count) 0)
        (let* ((d (slot-value 'direction))
               (n (length d))
               (a (abs (slot-value 'angle))))
          (setf (slot-value 'tour-count)
                (random (floor (/ pi (* 2 a)))))
          (setf (slot-value 'tour-trans) 
                (make-rotation d (normal-rand n) a))))
  (setf (slot-value 'direction)
        (matmult (slot-value 'tour-trans) (slot-value 'direction)))
  (send self :set-image)
  (setf (slot-value 'tour-count) (- (slot-value 'tour-count) 1)))

(defmeth ncheck-plot-proto :do-idle () (send self :tour-step))

(defmeth ncheck-plot-proto :menu-template ()
  (append (call-next-method) (list (send run-item-proto :new self))))

(defmeth ncheck-plot-proto :isnew (f d)
  (setf (slot-value 'function) f)
  (setf (slot-value 'direction) d)
  (setf (slot-value 'xvals) (rseq -3 3 7))
  (call-next-method 2)
  (send self :range 0 -3 3)
  (send self :range 1 0 1.2)
  (send self :x-axis t nil 7)
  (send self :y-axis nil)
  (send self :set-image))
