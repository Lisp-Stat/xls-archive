;;;
;;;   Plot control protos
;;;

;;; heteroscedasticity control proto

(defproto hetero-control-proto '(text) () graph-control-proto)
(defmeth hetero-control-proto :isnew (loc text)
  (call-next-method :location loc :title "Change Model")
  (setf (slot-value 'text) text))
(defmeth hetero-control-proto :redraw ()
  (call-next-method)
  (when (slot-value 'text) (send (send self :graph) 
                  :draw-text (slot-value 'text) 
                  (- (send (send self :graph) :canvas-width) 10) 10 2 0)))
(defmeth hetero-control-proto :do-action (modifier)
  (let* ((graph (if (send self :has-slot 'graph) (send self :graph) nil)))
    (when graph (send graph :hetero-change-model)
          (send graph :redraw))))

(defproto hetero2-control-proto '() () checkbox-control-proto)
(defmeth hetero2-control-proto :isnew (loc)
  (call-next-method  :location loc :title "Abs. Residuals"))
(defmeth hetero2-control-proto :do-action () 
  (let* ((graph (send self :graph)))
    (send graph :update-res (slot-value 'selected))))

;;;
;;; scale proto -toggles abc/aaa scaling in 3-D plots
;;;

(defproto scale-control-proto '() () checkbox-control-proto)
(defmeth scale-control-proto :isnew (loc)
  (call-next-method  :location loc :title "Fixed Scaling"))
(defmeth scale-control-proto :do-action () 
  (let* ((graph (send self :graph)))
   (if (slot-value 'selected) (send graph :scale-type 'fixed)
      (send graph :scale-type 'variable))))

;;; installation method

(defmeth graph-proto :install-scale-control ()
  (send self :add-control (send scale-control-proto :new 
                        (send self :locate-next-control))))


;;;
;;; orthogonalize axes proto --- for spin plots
;;;

(defproto ortho-control-proto '(axes) () checkbox-control-proto)
(defmeth ortho-control-proto :isnew (loc)
  (call-next-method  :location loc :title "Orth. Axes")
  (setf (slot-value 'axes) '(0 2)))
(defmeth ortho-control-proto :do-action () 
  (let* ((graph (send self :graph))
         (p (send graph :num-point-variables)))
    (if (null (slot-value 'axes)) 
        (setf (slot-value 'axes) (remove 1 (iseq p))))
   (if (slot-value 'selected) (send graph :ortho (slot-value 'axes))
      (send graph :original-data))))
(defmeth ortho-control-proto :option-click ()
  (let* ((graph (send self :graph))
         (p (send graph :num-point-variables))
         (names (coerce (send graph :variable-labels) 'list))
         (axes (first (choose-subset-dialog "Space to Orthogonalize" names))))
    (when (> (length axes) 1) (setf (slot-value 'axes) axes))))

;;;
;;; Sequential orthogonalization
;;;


(defmeth spin-proto :ortho (&optional vars)
"
Message args: (&optional vars)
Replaces variables named in the list VARS with sequentially orthogonalized 
variables in the order given in VARS.  If VARS is nil, the entire plot is 
replaced with orthogonalized variables.  Orthogonalization is via the 
QR decomposition."
(let* ((p (send self :num-point-variables))
       (n (send self :num-points))
       (vars (if vars vars (iseq p)))
       (rep (repeat (list (iseq n)) (length vars)))
       (data (send self :point-coordinate vars rep))
       (pr (project data :intercept nil
              :included (send self :point-showing (iseq n))))
       (orthdata (column-list (send pr :q)))
       (full-d-orthdata (if (eq (length orthdata) (length vars)) 
             orthdata
             (append orthdata (repeat (list (repeat 0 n)) 
                       (- (length vars)
                          (length orthdata)))))))
  (send self :add-slot 'data (send self :point-coordinate
                                 (iseq p) (repeat (list (iseq n)) p)))
  (send self :start-buffering)
  (send self :point-coordinate vars rep full-d-orthdata)
  (send self :adjust-to-data)
  (send self :buffer-to-screen)))

(defmeth spin-proto :original-data ()
"
Message args: ()
Returns the plot of the original data. Useful for undoing an orthogonalization."
(send self :start-buffering)
(dotimes (i (send self :num-variables))
         (send self :point-coordinate i 
               (iseq 0 (- (send self :num-points) 1))
               (nth i (send self :slot-value 'data))))
(send self :adjust-to-data)
(send self :buffer-to-screen))

(defmeth spin-proto :install-ortho-control ()
  (send self :add-control (send ortho-control-proto :new 
                                (send self :locate-next-control))))

;;;;
;;;; curve controls for 2-d plots --- general prototype
;;;;

(defproto curve-control-proto '(smoother args color lines) 
  () checkbox-control-proto)
(defmeth curve-control-proto :isnew (loc title smoother args color)
  (call-next-method  :location loc :title title) 
  (send self :smoother smoother)
  (send self :args args)
  (send self :color color)
  self)
(defmeth curve-control-proto :smoother (&optional new)
  (when new (setf (slot-value 'smoother) new))
  (slot-value 'smoother))
(defmeth curve-control-proto :args (&optional new)
  (when new (setf (slot-value 'args) new))
  (slot-value 'args))
(defmeth curve-control-proto :color (&optional new)
  (when new (setf (slot-value 'color) new))
  (when (send self :graph)
        (send (send self :graph) :get-color (slot-value 'color))))
(defmeth curve-control-proto :do-action () 
  (when (send self :graph) (send self :draw-curve))
  (send self :bitmap (slot-value 'selected))
  (send self :redraw))
(defmeth curve-control-proto :draw-curve ()
   (let* ((graph (send self :graph)))
         (cond ((slot-value 'selected)
                  (let* ((color (send self :color))
                         (args (send self :args))
                         (smoother (send self :smoother))
                         (vals (apply #'funcall smoother args))
                         (nl (send graph :num-lines)))
                 (send graph :add-lines vals :color color)
                 (setf (slot-value 'lines)
                            (iseq nl (send graph :num-lines)))))
                (t
                  (send graph :linestart-masked (slot-value 'lines) t)
                  (send graph :redraw-content)))
         (slot-value 'selected)))


;;;
;;;   Instances of curve-control-proto:  Installation
;;;

;;; Install fit OLS line

(defmeth scatterplot-proto :install-OLS ()
  (let* ((lin #'(lambda (args)
                 (let* ((n (send self :points-showing))
                        (d (send self :point-coordinate '(0 1) (list n n)))
                        (r (regression-model (select d 0) (select d 1)
                                             :print nil))
                        (b (send r :coef-estimates))
                        (x (range (first d))))
                       (list x (+ (first b) (* (second b) x))))))
         (control (send curve-control-proto :new
                (send self :locate-next-control) "OLS fit"
                lin (list nil) (select *colors* 6))))
  (send self :start-next-frame #'(lambda () (send self :clear-lines)))
  (send self :finish-next-frame #'(lambda () 
                           (when (send control :slot-value 'selected)
                                 (send control :draw-curve))))
  (send self :add-control control)
  control))

;;;  install zero line button

(defmeth scatterplot-proto :install-zero-line ()
  (let* ((zero #'(lambda (args) (list (send self :range 0) '(0 0))))
         (control (send curve-control-proto :new
                (send self :locate-next-control) "Zero line"
                zero (list nil) (select *colors* 1))))
  (send self :finish-next-frame #'(lambda () 
                           (when (send control :slot-value 'selected)
                                 (send control :draw-curve))))
  (send self :add-control control)
  control))

;;;
;;; install join points
;;;

(defmeth scatterplot-proto :install-join-points ()
  (let* ((join #'(lambda (args) (send self :join-points)))
         (control (send curve-control-proto :new
                (send self :locate-next-control) "Join points"
                join (list nil) (select *colors* 6))))
  (send self :finish-next-frame #'(lambda () 
                           (when (send control :slot-value 'selected)
                                 (send control :draw-curve))))
  (send self :add-control control)
  control))

(defmeth scatterplot-proto :join-points ()
   (let* ((n (iseq (send self :num-points)))
          (x (send self :point-coordinate 0 n))
          (y (send self :point-coordinate 1 n))
          (or (order x)))
         (list (select x or) (select y or))))

(defmeth scatterplot-proto :draw-join-points ()
  (when (null (send self :has-slot 'join-points))
        (send self :add-slot 'join-points nil))
  (cond ((null (slot-value 'join-points))
         (let* ((nl (send self :num-lines))
               (send self :add-lines (send self :join-points))
               (setf (slot-value 'join-points) 
                     (iseq nl (send self :num-lines))))))
        (t
         (send self :linestart-masked (slot-value 'join-points) t)
         (send self :redraw-content)
         (setf (slot-value 'join-points) nil)))
  (slot-value 'join-points))

;;;
;;; added variable plot controls
;;;

(defproto avp-control-proto '() () curve-control-proto)
(defmeth avp-control-proto :do-action ()
  (call-next-method )
  (let* ((graph (send self :graph)))
    (send graph :redraw)))
(defmeth avp-control-proto :draw-curve ()
  (let* ((graph (send self :graph)))
        (cond ((slot-value 'selected)
               (let* ((nl (send graph :num-lines))
                      (args (send self :args))
                      (color (send self :color)))
                     (send graph :start-buffering)
                     (apply #'send graph :p-contour args)
                     (setf (slot-value 'lines) 
                           (iseq nl (send graph :num-lines)))
                     (send graph :linestart-color (slot-value 'lines) color)
                     (send graph :buffer-to-screen)))
              (t
               (send graph :linestart-masked (slot-value 'lines) t)
               (send graph :redraw-content)))
        (slot-value 'selected)))

(defmeth scatterplot-proto :avp-controls ()
  (let* ((con1 (send avp-control-proto :new
                        (send self :locate-next-control) "CaseWt Perturb"
                        #'(lambda (a) ())
                        (list "case-weights") 'blue))
         (con2 (send avp-control-proto :new
                        (send self :locate-next-control) "Pred Perturb"
                        #'(lambda (a) ())
                        (list "predictors") 'red)))
        (send self :add-control con1)
        (send self :add-control con2)
        (send self :finish-next-frame
              #'(lambda () (when (send con1 :slot-value 'selected)
                                 (send con1 :draw-curve))
                        (when (send con2 :slot-value 'selected)
                              (send con2 :draw-curve))))
        (list con1 con2)))
;;;
;;;  Contour methods for added variable plots
;;;

(defmeth graph-proto :p-contour (type &optional (c .57735))
"
Message args: (type &optional (c .57735))
Adds perturbation contours to an added variable plot of type type.  
Used by the avp-controls method."
  (let* ((inc (which (send (slot-value 'owner) :included)))
         (which (iseq 0 (1- (send self :num-points))))
         (num (send self :num-lines))
         (det (if (send self :has-slot 'detrended)
                  (slot-value 'detrended) nil))
         (xx (select (send self :point-coordinate 0 which) inc))
         (yy (select (send self :point-coordinate 1 which) inc))
         (c (* c (sqrt (sum (^ (if (equal type "case-weights")
                                   (* (- yy (if det 0 xx)) xx)
                                   (- yy (* xx (if det 1 2))))
                                   2)))))
         (xr (send self :range 0))
         (yr (send self :range 1)))
    (flet ((f (x sign) (+ (if det 0 x) (/ (* c sign) x))))
      (send self :start-buffering)
      (cond
        ((equal type "case-weights")
         (send self :add-function #'(lambda (x) (f x 1)) 
            (first xr) (/ c (first yr)))
         (send self :add-function #'(lambda (x) (f x 1)) 
            (/ c (second yr)) (second xr)) 
         (send self :add-function #'(lambda (x) (f x -1)) 
            (first xr) (/ c (- (second yr))))
         (send self :add-function #'(lambda (x) (f x -1)) 
            (/ c (- (first yr))) (second xr)))
       (t
        (send self :abline    c  (if det 1 2))
        (send self :abline (- c) (if det 1 2))))
      (send self :redraw-content)
      (send self :buffer-to-screen))))

;;;;
;;;; smoother-control-proto -- inherits from curve control proto
;;;;

(defproto smoother-control-proto '() () curve-control-proto)
(defmeth smoother-control-proto :shift-click () 
  (let* ((graph (send self :graph))
         (r (standard-deviation (send graph :point-coordinate 0 
                                      (iseq (send graph :num-points)))))
         (action #'(lambda (w) 
            (send graph :start-buffering)
            (when (send self :selected)
                  (send self :selected nil)
                  (send self :draw-curve))
            (send self :selected t)
            (send self :args (list (* r w)))
            (send self :draw-curve)
            (send graph :buffer-to-screen)
            (send self :redraw)))) 
    (send graph :add-subordinate
          (interval-slider-dialog (list .1 2.0)
                                  :title (send self :title)
                                  :text "Width/SD" :action action
                                  :points 20))))


;;; smoother slider control

(defproto smoother-slider-control-proto '(smoother args color lines menu) 
  () slider-control-proto)
(defmeth smoother-slider-control-proto :isnew (seq display loc length index
                                                   title smoother args color)
  (call-next-method  seq :location loc :title title :length length
                     :length length :index index :display display) 
  (send self :smoother smoother)
  (send self :args args)
  (send self :color color)
  self)
(defmeth smoother-slider-control-proto :smoother (&optional new)
  (when new (setf (slot-value 'smoother) new))
  (slot-value 'smoother))
(defmeth smoother-slider-control-proto :args (&optional new)
  (when new (setf (slot-value 'args) new))
  (slot-value 'args))
(defmeth smoother-slider-control-proto :color (&optional new)
  (when new (setf (slot-value 'color) new))
  (when (send self :graph)
        (send (send self :graph) :get-color (slot-value 'color))))
(defmeth smoother-slider-control-proto :do-action (j) 
  (when (send self :graph) 
        (send self :args (list (select (send self :display) j)))
        (send self :draw-curve)))
(defmeth smoother-slider-control-proto :draw-curve ()
   (let* ((graph (send self :graph)))
         (when (slot-value 'lines)
               (send graph :linestart-masked (slot-value 'lines) t))
         (when (> (send self :index) 0)
                  (let* ((color (send self :color))
                         (args (send self :args))
                         (smoother (send self :smoother))
                         (vals (apply #'funcall smoother args))
                         (nl (send graph :num-lines)))
                 (send graph :add-lines vals :color color)
                 (setf (slot-value 'lines)
                            (iseq nl (send graph :num-lines)))))
         (send graph :redraw-content)
         ))
(defmeth smoother-slider-control-proto :shift-click () (sysbeep))


;;;
;;; instances
;;;

(defmeth scatterplot-proto :install-m-est ()
  (let* ((lin #'(lambda (args)
                 (let* ((n (send self :points-showing))
                        (d (send self :point-coordinate '(0 1) (list n n)))
                        (b (robust-coefs (list (first d)) (second d)
                                         (make-wf 'huber args)))
                        (x  (range (first d))))
                       (list x (+ (first b) (* (second b) x))))))
         (control (send smoother-slider-control-proto :new
                (iseq 13) (combine nil (rseq .25 3 12))
                (send self :locate-next-control :height 2) 
                (send self :slider-width) 0 "M-est fit"
                lin (list 1.345) (select *colors* 5))))
  (send self :start-next-frame #'(lambda () (send self :clear-lines)))
  (send self :finish-next-frame #'(lambda () (send control :draw-curve)))
  (send self :add-control control)
  control))

(defun robust-coefs (x y wf &key 
                       (weights (repeat 1 (length y))) 
                       (tol .0001)
                       (count-limit 40))
  (let ((x (if (matrixp x) x (apply #'bind-columns x))))
    (labels ((as-list (x) (coerce (compound-data-seq x) 'list))
             (rel-err (x y) 
               (mean (/ (abs (- x y)) (+ 1 (abs x)))))
             (reg-coefs (weights)
               (let* ((m (make-sweep-matrix x y weights))
                      (p (array-dimension x 1)))
                 (as-list
                  (select (first (sweep-operator m (iseq 1 p)))
                          (1+ p)
                          (iseq 0 p)))))
             (fitvals (beta)
               (+ (first beta) (matmult x (rest beta))))
             (improve-guess (beta)
               (let* ((resids (- y (fitvals beta)))
                      (scale (/ (median (abs resids)) .6745))
                      (wts (funcall wf (/ resids scale))))
                 (reg-coefs wts)))
             (good-enough-p (last beta count)
               (if (> count count-limit)
                   (format t "Iteration limit exceeded~%"))
               (or (> count count-limit)
                   (and last (< (rel-err beta last) tol)))))
      (do ((last nil beta)
           (count 0 (+ count 1))
           (beta (reg-coefs weights) (improve-guess beta)))
          ((good-enough-p last beta count) beta)))))

(defun make-wf (name &optional
                     (k (case name
                              (biweight 4.685)
                              (cauchy 2.385)
                              (huber 1.345))))
  #'(lambda (r) 
    (let ((u (abs (/ r k))))
      (case name
            (biweight (^ (- 1 (^ (pmin u 1) 2)) 2))
            (cauchy (/ 1 (+ 1 (^ u 2))))
            (huber (/ 1 (pmax u 1)))))))

;;;
;;; instances of smoother-slider-control-proto
;;;

(defproto kernel-slider-control-proto '() () smoother-slider-control-proto)

(defmeth kernel-slider-control-proto :shift-click ()(sysbeep))

(defmeth kernel-slider-control-proto :do-action (j) 
  (when (send self :graph) 
        (send self :args (list (select (send self :display) j)
                               (second (send self :args))))
        (send self :draw-curve)))

(defmeth scatterplot-proto :install-kernel ()
  (let* ((smoother #'(lambda (w type)
                             (let* ((n (send self :points-showing))
                                    (d (send self :point-coordinate '(0 1)
                                                   (list n n))))
                                   (kernel-smooth (first d) (second d)
                                        :type type :width 
                                        (* w (apply #'-
                                             (reverse (range (first d)))))))))
         (control (send kernel-slider-control-proto :new
                        (iseq 13) (combine nil .01 .03 (rseq 0.05 .5 10) )
                        (send self :locate-next-control :height 2) 
                        (send self :slider-width) 0  "GaussSmooth"
                        smoother (list 0 'g) (select *colors* 4))))
    (send self :start-next-frame #'(lambda () (send self :clear-lines)))
    (send self :finish-next-frame #'(lambda () 
                                 (send control :draw-curve)))
    (send self :add-control control)
    control))

(defproto lowess-slider-control-proto '() () kernel-slider-control-proto)

(defmeth scatterplot-proto :install-lowess ()
  (let* ((smoother #'(lambda (w steps)
                             (let* ((n (send self :points-showing))
                                    (d (send self :point-coordinate '(0 1)
                                                   (list n n))))
                                   (lowess (first d) (second d)
                                                  :f w :steps steps))))
         (control (send lowess-slider-control-proto :new
                (iseq 11) (combine nil (rseq 0.1 1 10))
                (send self :locate-next-control :height 2) 
                (send self :slider-width) 0 "Lowess"
                smoother (list 0 0) (select *colors* 2))))
  (send self :start-next-frame #'(lambda () (send self :clear-lines)))
  (send self :finish-next-frame #'(lambda () 
                                 (send control :draw-curve)))
  (send self :add-control control)
  control))

;;; install kernel density estimate for histograms

(defmeth histogram-proto :install-kernel-density ()
  (let* ((smoother #'(lambda (w type)
                       (let* ((d (send self :point-coordinate 0
                                       (send self :points-showing))))
                         (kernel-dens d :type type :width 
                                      (* w (standard-deviation d))))))
         (control (send kernel-slider-control-proto :new
                        (iseq 21) (combine nil (rseq 0.2 4 20) )
                        (send self :locate-next-control :height 2) 
                        (send self :slider-width) 0  "GaussKerDens"
                        smoother (list 0 'g) (select *colors* 4))))
    (send self :add-control control)
    (send self :start-next-frame #'(lambda () (send self :clear-lines)))
    (send self :finish-next-frame #'(lambda () (send control :draw-curve)))
    (defmeth control :shift-click () (sysbeep))
    control))

                     
