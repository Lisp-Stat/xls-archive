(load-example "addhandrotate")
(load "plotcontrols")
(load "buttons")

(load-data "diabetes")
(load-data "iris")
(load "stackloss")
(load "abrasion")
(load "smallplaces")
(load "laser")

;;
;; Demo Menu Variable
;;

(defvar *demo-menu* nil)
(if *demo-menu* (send *demo-menu* :remove))

(def times '(9 13 13 18 23 28 31 34 45 48 161
             5 5 8 8 12 16 23 27 30 33 43 45))
(def status '(1 1 0 1 1 0 1 1 0 1 0 1 1 1 1 1 0 1 1 1 1 1 1))

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
  (flet ((sphere-rand (n)
           (let* ((z (normal-rand n))
		  (r (sqrt (sum (^ z 2)))))
	     (if (< 0 r) (/ z r) (repeat (/ (sqrt n)) n)))))
    (let ((p (apply #'spin-plot args)))
      (send p :add-slot 'tour-count -1)
      (send p :add-slot 'tour-trans nil)
      (defmeth p :tour-step ()
	(when (< (slot-value 'tour-count) 0)
	      (let ((vars (send self :num-variables))
		    (angle (send self :angle)))
		(setf (slot-value 'tour-count) (random 20))
		(setf (slot-value 'tour-trans) 
		      (make-rotation (sphere-rand vars)
				     (sphere-rand vars)
				     angle))))
	(send self :apply-transformation (slot-value 'tour-trans))
	(setf (slot-value 'tour-count) (- (slot-value 'tour-count) 1)))
      (defmeth p :do-idle () (send self :tour-step))
      (send (send p :menu) :append-items (send run-item-proto :new p))
      p)))

(defun sphere-rand (n &optional (dim 3))
  (mapcar #'(lambda (k) 
              (do ((x 
                    (- (* 2 (uniform-rand k)) 1) 
                    (- (* 2 (uniform-rand k)) 1)))
                  ((< (sum (* x x)) 1) x))) (repeat dim n)))

(defun make-sine-demo ()
  (let ((x (uniform-rand 100)))
    (plot-points x (sin (* 20 pi x)))))
	
(defun make-bar-demo ()
  (let ((bar (spin-plot (let* ((x1 (* 20 (uniform-rand 40)))
			       (x2 (normal-rand 40))
			       (y (normal-rand 40)))
			  (list x1 y x2))
			:variable-labels '("X1" "Y" "X2")
			:scale 'fixed)))
    (send bar :depth-cuing nil)
    (send bar :redraw)))

(defun make-laser-demo () (spin-plot laser))

(defun make-abrasion-demo ()
  (spin-plot (list tensile-strength abrasion-loss hardness) 
	     :variable-labels '("T" "A" "H")))

(defun make-spheres-demo ()
  (let ((x (sphere-rand 100)))
    (def p1 (spin-plot (transpose x)))
    (def p2 (spin-plot (transpose (mapcar 
                                   #'(lambda (x) 
                                       (let ((n (sqrt (sum (* x x))))) 
                                         (* (+ .8 (* .2 n)) (/ x n)))) x))))
    (send p2 :location 250 21)))

(defun make-randu-demo ()
  (load-data "randu")
  (spin-plot randu))

(defun make-diabetes-demo ()
  (load-data "diabetes")
  (spin-plot (select diabetes '(0 1 2))
	     :variable-labels (select dlabs '(0 1 2))))

(defun make-surface-demo (&optional (n 6))
  (flet ((bn (x y) (exp (* -0.5 (+ (^ x 2) (^ y 2) (* x y))))))
    (let ((p (spin-function #'bn -2.5 2.5 -2.5 2.5 :num-points n)))
      (send p :mouse-mode 'hand-rotate))))

(defun make-links-demo ()
  (histogram hardness :title "Hardness")
  (plot-points tensile-strength abrasion-loss
	       :variable-labels '("Tensile Strength" "Abrasion Loss")))

(setf *demo-menu* (send menu-proto :new "Demos"))
(send *demo-menu* :append-items
      (send menu-item-proto :new "Sine" :action
            #'(lambda () (make-sine-demo)))
      (send menu-item-proto :new "Bar" :action
            #'(lambda () (make-bar-demo)))
      (send menu-item-proto :new "Laser" :action
            #'(lambda () (make-laser-demo)))
      (send menu-item-proto :new "Abrasion" :action
            #'(lambda () (make-abrasion-demo)))
      (send menu-item-proto :new "Spheres" :action
            #'(lambda () (make-spheres-demo)))
      (send menu-item-proto :new "Randu" :action
            #'(lambda () (make-randu-demo)))
      (send menu-item-proto :new "Diabetes" :action
            #'(lambda () (make-diabetes-demo)))
      (send menu-item-proto :new "Surface" :action
            #'(lambda () (make-surface-demo)))
      (send menu-item-proto :new "Linked Plots" :action
            #'(lambda () (make-links-demo))))

(send *demo-menu* :append-items
      (send dash-item-proto :new)
      (send menu-item-proto :new "Regression" :action
	    #'(lambda ()
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
		(send p :mouse-mode 'point-moving))))

(send *demo-menu* :append-items
      (send dash-item-proto :new)
      (send menu-item-proto :new "Diabetes Interp" :action
	    #'(lambda () (interp-plot diabetes)))
      (send menu-item-proto :new "Iris Interp" :action
            #'(lambda () (interp-plot iris)))
      (send menu-item-proto :new "Stack Loss Interp" :action
            #'(lambda () (interp-plot (list air conc temp loss))))
      (send menu-item-proto :new "Places Interp" :action
            #'(lambda () (interp-plot (list longitude latitude
					    climate-terrain housing)))))

(send *demo-menu* :append-items
      (send dash-item-proto :new)
      (send menu-item-proto :new "Diabetes Tour" :action
            #'(lambda () (tour-plot diabetes :variable-labels dlabs)))
      (send menu-item-proto :new "Iris Tour" :action
            #'(lambda () (tour-plot iris)))
      (send menu-item-proto :new "Stack Loss Tour" :action
            #'(lambda () (tour-plot (list air conc temp loss)
				    :variable-labels
				    '("Air" "Conc" "Temp" "Loss")))))

(setf *sample-size* 100)

(defun make-d3 (n &optional (p 0))
  (transpose (mapcar #'(lambda (x u) 
                         (* (- 1 (* p u)) (/ x (sqrt (sum (* x x))))))
                     (normal-rand (repeat 4 n))
                     (uniform-rand n))))

(defun make-d3* (n)
  (let ((d3 (make-d3 n)))
    (transpose (mapcar #'(lambda (x y) (* (^ x (/ 1 3)) y))
                       (uniform-rand n) 
                       (transpose d3)))))

(defun make-4d-demo ()
  (if (< (random 1.0) 0.5)
      (let* ((d (make-d3 *sample-size* 0.2))
	     (p (tour-plot d)))
	(send (send p :menu) :append-items
	      (send menu-item-proto :new "Dimension?" :action
		    #'(lambda ()
			(message-dialog "3D - Surface of 4D Sphere")))))
      (let* ((d (make-d3* *sample-size*))
             (p (tour-plot d)))
        (send (send p :menu) :append-items
              (send menu-item-proto :new "Dimension?" :action
                    #'(lambda () (message-dialog "4D Ball")))))))

(send *demo-menu* :append-items
      (send menu-item-proto :new "4D Sphere" :action
	    #'(lambda () (make-4d-demo))))

;; *** sphere/ball
(send *demo-menu* :install)
