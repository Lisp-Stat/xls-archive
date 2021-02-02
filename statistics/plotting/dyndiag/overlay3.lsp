;;;
;;;  Extract values on an axis and save as a variable
;;;

(defproto extract-axis-proto '(axis) () graph-control-proto)
(defmeth extract-axis-proto :isnew (loc title axis)
  (call-next-method :location loc :title title)
  (setf (slot-value 'axis) axis))
(defmeth extract-axis-proto :do-click (x y a b)
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (loc-x (first loc))
         (loc-y (+ 5 (second loc))))
    (when (and (< loc-x x (+ loc-x 10)) (< loc-y y (+ loc-y 10)))
          (send self :do-action (list a b))
          t)))

(defmeth extract-axis-proto :do-action (first)
  (let ((str (get-string-dialog "Name saved quantity" :initial "PLOT-DIR")))
  (if str
	(set  (intern (string-upcase str))
	      (send (send self :graph) :point-transformed-coordinate
		    (slot-value 'axis) 
		    (iseq (send (send self :graph) :num-points)))))))

(defmeth spin-proto :install-extract-axis (axis)
  (let* ((title (if (= 0 axis) "Extract Horiz" "Extract Vert"))
         (control (send extract-axis-proto :new
                        (send self :locate-next-control)
                        title axis)))
        (send self :add-control control)
        (send self :redraw)
        control))


;;;
;;;  Spin off a 2-D plot from the 3-D plot
;;;

(defproto extract-2d-proto '() () graph-control-proto)
(defmeth extract-2d-proto :isnew (loc title )
  (call-next-method :location loc :title title))
(defmeth extract-2d-proto :do-click (x y a b)
"This override does not have a while button down action."
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (loc-x (first loc))
         (loc-y (+ 5 (second loc))))
    (when (and (< loc-x x (+ loc-x 10)) (< loc-y y (+ loc-y 10)))
          (send self :do-action (list a b))
          t)))

(defmeth extract-2d-proto :do-action (first)
  (let* ((plot (send self :graph))
         (n (send plot :num-points))
         (reg (if (send plot :has-slot 'owner)
                  (send plot :slot-value 'owner)
                  nil))
         (d (send plot :point-transformed-coordinate
                   '(0 1) (repeat (list (iseq n)) 2)))
         (graph (cond (reg (apply #'send reg :plot d))
                      (t (let ((p (apply #'plot-points d)))
                              (send p :plot-controls)
                              p)))))
        (send graph :x-axis t t)
        (send graph :y-axis t t)
        (send graph :title "Extract from spinning plot")
        (send graph :variable-label '(0 1) '("Horizontal" "Vertical"))
        graph))

(defmeth spin-proto :install-extract-2d ()
  (let* ((title "Extract 2-D Plot")
         (control (send extract-2d-proto :new
                        (send self :locate-next-control)
                        title )))
        (send self :add-control control)
        (send self :redraw)
        control))
;;;;
;;;; Miscellaneous controls
;;;;


;;; home control sends a 3-D plot "Home"

(defproto home-control-proto '(v) () graph-control-proto)
(defmeth home-control-proto :isnew (loc)
  (call-next-method :location loc :title "Home"))
(defmeth home-control-proto :do-click (x y a b)
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (loc-x (first loc))
         (loc-y (+ 5 (second loc))))
    (when (and (< loc-x x (+ loc-x 10)) (< loc-y y (+ loc-y 10)))
          (send self :do-action (list a b))
          t)))
(defmeth home-control-proto :do-action (first) 
  (let* ((graph (send self :graph))) 
    (send graph :add-slot '4d-position -1)
    (when graph (send graph :transformation nil)) 
    (when (= 4 (send graph :num-point-variables)) (send graph :draw-title))))

(defmeth graph-proto :install-home-control ()
  (send self :add-control (send home-control-proto :new
              (send self :locate-next-control))))

;;; transformation controls

(defproto transform-control-proto '(axis data now-computing) () 
  slider-control-proto)
(defmeth  transform-control-proto :isnew (loc lam pow &key graph 
                                                 length 
                                                 (axis 0)
                                                 (title "Power")) 
  (call-next-method lam :location loc :graph graph :length length
                    :display (* .01 (round (* 100 pow)))
                    :title title :index 8)
  (send self :axis axis))
(defmeth transform-control-proto :axis (&optional (new nil set))
  (when set (setf (slot-value 'axis) new))
  (slot-value 'axis))

(defmeth transform-control-proto :data (&key reset)
  (cond (reset (setf (slot-value 'data) nil))
        ((slot-value 'data) (slot-value 'data))
        (t (setf (slot-value 'data)
                  (send (send self :graph) :point-coordinate
                        (send self :axis) 
                        (iseq (send (send self :graph) :num-points))))))
  (slot-value 'data))

(defmeth  transform-control-proto :do-action (index) 
  (when (send self :graph)
        (let* ((graph (send self :graph))
               (n (send graph :num-points))
               (data (send self :data))
               (pow (select (send self :display) index))
               (new (send self :transform data pow)))
          (setf (slot-value 'now-computing) t)
          (send graph :start-buffering)
          (send graph :draw-next-frame (list (slot-value 'axis))
                  (list new))
          (send graph :adjust-to-data)
          (send graph :buffer-to-screen)
          (setf (slot-value 'now-computing) nil))))

(defmeth transform-control-proto :transform (x lambda)
  (when (<= (min x) 0)
        (error "Negative data"))
  (cond ((= lambda 0) (log x))
        ((= lambda 1) x)
        (t (/ (- (^ x lambda) 1) lambda))))

;;; installation method

(defmeth graph-proto :install-transform-control () 
  (when (null (send self :has-slot 'overlay-loc))
        (call-method graph-proto :plot-controls)) 
  (let* ((p (send self :num-point-variables))
         (n (iseq (send self :num-points)))
         (names (coerce (send self :variable-labels) 'list))
         (names (if names (mapcar #'(lambda (n) (substr n 0 6)) names) 
             (mapcar #'(lambda (a) (format nil "Var~a" a)) (iseq p)))))
    (mapcar #'(lambda (p name) 
                (when (> (min (send self :point-coordinate p n)) 0) 
                      (let* ((control (send transform-control-proto :new
                                  (send self :locate-next-control :height 2)
                                  (iseq 13) (rseq -1 2 13)
                                  :length (send self :slider-width)
                                  :axis p :title name)))
;                           (send self :start-next-frame #'(lambda ()
;                                (when (null (send control :slot-value 
;                                                  'now-computing))
;                                     (send control :data :reset t)
;                                     (send control :update-index 8))))
                            (send self :add-control control))))
                 (reverse (iseq p)) (reverse names))
    (when (equal (send self :slot-value 'proto-name) 'scatmat-proto)
          (defmeth self :redraw-content ()
                   (call-next-method)
                   (mapcar #'(lambda (ov) (send ov :redraw))
                           (slot-value 'overlays)))
          (defmeth self :redraw ()
            (call-next-method)
            (mapcar #'(lambda (ov) (send ov :redraw)) 
                    (slot-value 'overlays))))))
            
            
;;; histogram controls

(defproto hist-control-proto '() () slider-control-proto)
(defmeth  hist-control-proto :do-action (j)) 
(defmeth hist-control-proto :index (&optional (new nil set))
  (when set
      (let* ((new (max (send self :min) (min new (send self :max))))) 
        (setf (slot-value 'index) new)
        (send self :draw-indicator)
        (send self :do-action (elt (send self :sequence) new))
        (if (send self :graph) (send (send self :graph) 
                                     :num-bins (+ 2 new)))))  
  (slot-value 'index))

(defmeth graph-proto :dynamic-plot-control (seq display)
  (send self :add-control (send dynamic-plot-control-proto :new
                   (send self :locate-next-control :height 2)
                        seq display
                        :length (send self :slider-width))))


;;; dynamic plot slider

(defproto dynamic-plot-control-proto () () slider-control-proto)
(defmeth  dynamic-plot-control-proto :isnew (loc lam pow &key graph 
                                                 (length 80)
                                                 (title "Power")) 
  (call-next-method lam :location loc :graph graph :length length
                    :display (* .01 (round (* 100 pow)))
                    :title title))
(defmeth  dynamic-plot-control-proto :do-action (j) 
  (when (slot-value 'graph)
        (let* ((graph (send self :graph)))
          (send graph :draw-next-frame '(0 1)
                (list (select (send graph :slot-value 'data-x) j) 
                      (select (send graph :slot-value 'data-y) j))))))


(defproto popup-menu-control-proto '(menu) () graph-control-proto)

(defmeth popup-menu-control-proto :isnew (loc &key (title "Popup Menu"))
  (call-next-method :location loc :title title))

(defmeth popup-menu-control-proto :menu ()
  (let* ((graph (send self :graph)))
    (when (null (slot-value 'menu))
        (let* ((menu (send menu-proto :new "Menu"))
               (item (send menu-item-proto :new "Home Away from Home"
                         :action #'(lambda () (sysbeep)))))
          (send menu :append-items item)
          (setf (slot-value 'menu) menu)))
    (slot-value 'menu)))

(defmeth popup-menu-control-proto :do-action (first)
  (let* ((menu (send self :menu))
         (loc (+ (send (send self :graph) :location) (send self :location))))
    (send menu :popup (+ 10 (first loc)) (+ 10 (second loc)))
    (send self :redraw)))

;;; 3-d residual control proto

(defproto 3d-control-proto '(menu) () popup-menu-control-proto)
(defmeth 3d-control-proto :isnew (loc)
  (call-next-method loc :title "Rotations Menu"))

(defmeth 3d-control-proto :menu ()
  (let* ((graph (send self :graph)))
    (when (null (slot-value 'menu))
        (let* ((menu (send menu-proto :new "Menu"))
               (yhat (send menu-item-proto :new "Rotate to Yhat"
                         :action #'(lambda () 
                            (send graph :rotate-to-yhat))))
               (e1 (send menu-item-proto :new "Rotate to e1"
                         :action #'(lambda () 
                            (send graph :rotate-to-e1))))
               (ares (send menu-item-proto :new "ARES rotation"
                         :action #'(lambda () 
                            (send graph :rotate-ares))))
               (coord (send menu-item-proto :new "Print Screen Coordinates"
                     :action #'(lambda () (send graph :screen-coordinates))))
               (int0 (send menu-item-proto :new "Move ... to Horizontal"
                     :action #'(lambda () (send graph :interchange 0))))
               (int1 (send menu-item-proto :new "Move ... to Vertical"
                     :action #'(lambda () (send graph :interchange 1))))
               (int2 (send menu-item-proto :new "Move ... to Out of Page"
                     :action #'(lambda () (send graph :interchange 2)))))
          (send menu :append-items  ares yhat e1 coord int0 int1 int2)
          (setf (slot-value 'menu) menu)))
    (slot-value 'menu)))


(defmeth spin-proto :rotate-to-yhat ()
"
Puts yhat12 on the x-axis."
  (let* ((p (send self :num-variables))
         (tr (if (send self :transformation) (send self :transformation)
                 (identity-matrix p)))
         (from (mapcar #'(lambda (j k) (if (or (= j 0) (= j 2)) k 0)) (iseq p) 
                       (* (send self :scale (iseq p)))))) 
    (send self :rotate-from-to (matmult tr from) 0)
    (send self :rotate-from-to (matmult (send self :transformation)
                       (mapcar #'(lambda (j) (if (= j 1) 1 0)) (iseq p))) 1)
    (send self :add-slot '4d-position -1)))

(defmeth spin-proto :rotate-to-e1 ()
"
Puts e1 on the y-axis by rotating from the current location of (1 1 0 ...) to (0 1 0 ...)."
  (let* ((p (send self :num-point-variables))
         (tr (if (send self :transformation) (send self :transformation)
                 (identity-matrix p)))
         (from (mapcar #'(lambda (j k) (if (< j 2) k 0)) (iseq p) 
                       (* (send self :scale (iseq p))))))
    (send self :rotate-from-to (matmult tr from) 1)
    (send self :rotate-from-to (matmult (send self :transformation)
                       (mapcar #'(lambda (k) (if-else (= k 2) 1 0)) (iseq p)))
          0)
    (send self :add-slot '4d-position -1)))

(defmeth spin-proto :rotate-ares (&optional (inc 20))
"
See Cook and Weisberg (1990), Three dimensional residual plots, 
Interface symposium, page 164."
  (when (null (send self :has-slot 'ares-direction))
        (send self :add-slot 'ares-direction '+)) 
  (cond ((eq (slot-value 'ares-direction) '+) (send self :ares-plus inc))
    (t (send self :ares-minus inc))))

(defmeth spin-proto :ares-minus (&optional (inc 20))
  (send self :add-slot 'ares-direction '+)
  (let* ((scale (send self :scale '(0 1 2)))
         (n (send self :num-point-variables))
         (a (nth 0 scale))
         (b (nth 1 scale))
         (c (nth 2 scale))
         (make-axis #'(lambda (j) (mapcar #'(lambda (k) (if-else (= j k) 1 0)) 
                                          (iseq n))))
         (theta-inc (/ (- (/ pi 2) (atan (/ c a))) inc)) ;;ok
         (m (make-rotation (funcall make-axis 0)
                           (- (funcall make-axis 2)) theta-inc))
         (vec (matmult (make-rotation (funcall make-axis 0)
                                      (- (funcall make-axis 2)) 
                                      (atan (/ c a)))
                      (coerce (funcall make-axis 0) 'vector))) 
         (theta-values (rseq (atan (/ c a)) (/ pi 2) (+ 1 inc))) ;;ok
         (theta-rot (make-rotation (funcall make-axis 0) (funcall make-axis 2)
                                   (- theta-inc)))
         (eta-values (atan (* (/ a b)
                              (- 1 (/ (/ c a) (tan theta-values)))))) ;;ok
         (eta-inc (- (cdr eta-values) (rmel inc eta-values)))
         (tr (if (send self :transformation) (send self :transformation)
                 (identity-matrix n)))) 
    (send self :apply-transformation 
          (make-rotation (matmult tr 
                          (coerce (* scale (+ (funcall make-axis 0) 
                                          (funcall make-axis 2))) 'vector))
                         (funcall make-axis 0)))
    (send self :apply-transformation 
          (make-rotation (matmult (send self :transformation) 
                                  (funcall make-axis 1)) 
                         (funcall make-axis 1)))
 ;   (send self :rotate-to-yhat)
    (dotimes (i inc)
             (setf vec (matmult m vec))
             (setf tr (matmult (make-rotation vec (funcall make-axis 1) 
                                              (nth i eta-inc)) 
                               theta-rot (send self :transformation)))
             (send self :transformation tr))))

(defmeth spin-proto :ares-plus (inc)
"
Message args: ()
Performs rotation from (e1,yh1) to (e yh) when yh2.1 is being plotted on the x-axis"
  (send self :add-slot 'ares-direction '-)
  (let* ((scale (send self :scale '(0 1 2)))
         (n (send self :num-point-variables))
         (a (nth 0 scale))
         (b (nth 1 scale))
         (c (nth 2 scale))
         (make-axis #'(lambda (j) (mapcar #'(lambda (k) (if-else (= j k) 1 0)) 
                                          (iseq n))))
         (theta-inc (/ (- (/ pi 2) (atan (/ c a))) inc)) ;;ok
         (m (make-rotation (- (funcall make-axis 2)) (funcall make-axis 0)
                          theta-inc)) 
         (vec (coerce (- (funcall make-axis 2)) 'vector))
         (theta-values (rseq (atan (/ c a)) (/ pi 2) (+ 1 inc))) ;;ok
         (eta-values (atan (* (/ a b)
                              (- 1 (/ (/ c a) (tan theta-values)))))) ;;ok
         (eta-inc (- (cdr eta-values) (rmel inc eta-values))))
    (send self :start-buffering)
    (send self :transformation nil)
    (send self :rotate-2 0 2 (- (/ pi 2)))
    (send self :rotate-2 1 2 (atan (/ a b)))
    (send self :buffer-to-screen)
    (dotimes (i inc)                           
             (send self :apply-transformation 
                   (make-rotation '(0 1 0) vec (nth i (reverse eta-inc))))
             (setf vec (matmult m vec))
             (send self :rotate-2 0 2 theta-inc))))


;;; print screen coordinates

(defmeth spin-proto :screen-coordinates ()
  (let* ((p (send self :num-point-variables)))
  (print-matrix (apply #'bind-rows 
                       (mapcar #'(lambda (j) (send self :name-on-axis j))
                               (iseq p))))))
                                         

(defmeth spin-proto :name-on-axis (j)
  (let* ((p (send self :num-point-variables))
         (tr (if (send self :transformation) (send self :transformation)
                 (identity-matrix p)))
         (pr (/ (coerce (coerce (select tr j (iseq p)) 'vector) 'list)
                 (send self :scale (iseq p))))) 
    (* .001 (round (* 1000 (/ pr (sqrt (inner-product pr pr))))))))

;;; interchange axes

(defmeth spin-proto :interchange (axis &key from)
  (let* ((graph self)
         (n (send graph :num-point-variables))
         (lab (send self :variable-label (iseq n)))
         (names (list "Move ... to Horizontal" "Move ... to Vertical"
                      "Move ... to Out of Page" "Move ... Off-screen"))
         (title (format nil "~a" (select names axis)))
         (from (if from from (choose-item-dialog title lab)))
         (s (send graph :scale (iseq n)))
         (trans (if (send graph :transformation) (send graph :transformation)
                    (identity-matrix n))))
    (send graph :redraw)
    (when from
          (when (numberp from)
                (setf from (mapcar #'(lambda (a) (if (= a from) 1 0)) 
                                  (iseq n))))
          (send graph :add-slot '4d-position -1) 
          (send graph :rotate-from-to (matmult trans (* from s)) axis))))

(defmeth spin-proto :rotate-from-to (from to)
"
Message args: (from to)
Rotates the plot from FROM in the direction TO.  from and two are
(1) lists of the same length as num-variables or
(2) intergers that label the axes, in which case rotation is in the plane of 
the corresponding axes.  By assumption, both from and to are in screen 
coordinates.  If they are in absolute coordinates, then they should 
be premultiplied as, for example, (matmult (send self :transformation) 
(coerce (* (send self :scale '(0 ...)) from) 'vector))."
  (labels ((make-vec (v) (if (listp v) v
                (mapcar #'(lambda (z) (if (eq v z) 1 0)) 
                        (iseq (send self :num-point-variables)))))
           (make-ang (from to) 
                     (let* ((ip1 (inner-product from to))
                            (ip (* (inner-product from from)
                                   (inner-product to to))))
                       (if (< ip 1.e-9) 0 (acos (/ ip1 (sqrt ip)))))))
    (let* ((from (make-vec from))
           (to   (make-vec to))
           (ang  (abs (make-ang from to)))
           (num-steps (max 2 (floor (/ (abs ang)
                                       (abs (send self :angle))))))
           (gamma (if (send self :transformation) (send self :transformation)
                   (identity-matrix (send self :num-point-variables))))
           (rot1 (make-rotation from to (/ ang num-steps))))
      (cond
        ((< (abs ang) 1.e-9) nil)           ;;;return if angle is 0
        (t (dotimes (j num-steps) (send self :apply-transformation rot1)))))))



;;;;
;;;; General Installation Methods
;;;;

(defmeth graph-proto :locate-next-control (&key (height 1) (absolute nil)
                                                (update t))
  (let ((a (slot-value 'overlay-loc))
        (size (cond (absolute height)
                    (t (+ 2 (* height (+  2 (send self :text-ascent) 
                                         (send self :text-descent))))))))
    (when update (setf (slot-value 'overlay-loc)
                       (list (first a) (+ (second a) size))))
    a))

(defmeth graph-proto :plot-control-left-margin ()
  (+ 35 (send self :text-width "Rem. Lin. Trend")))

(defmeth graph-proto :slider-width ()
  (+ 15 (send self :text-width "Rem. Lin. Trend")))
    
;;;
;;; plot-control methods
;;;

(defmeth graph-proto :plot-controls () 
  (let* ((margin (send self :margin))
         (left (send self :plot-control-left-margin))
         (top 12)
         (n (+ margin (list left top 0 0))))
    (apply #'send self :margin n)
    (send self :add-slot 'overlay-loc '(10 10))
    (apply #'send self :size (+ (send self :size) (list left top)))
#-macintosh (send self :dec-size)
    (send self :resize)))

;;; installation method

(defmeth histogram-proto :install-hist-proto ()
  (send self :add-control (send hist-control-proto :new (iseq 2 20)
                :location (send self :locate-next-control :height 2)
                :title "NumBins" :length (send self :slider-width))))


(defmeth histogram-proto :plot-controls ()
  (call-next-method)
  (send self :install-hist-proto)
  (send self :install-kernel-density)
  (send self :install-transform-control))

(defmeth scatterplot-proto :plot-controls ()
  (call-next-method)
  (send self :install-color-symbol-buttons)   
  (send self :install-toggle-linear-trend)
  (send self :install-OLS)
  (send self :install-zero-line)
  (send self :install-join-points)
  (send self :install-m-est)
  (send self :install-kernel)
  (send self :install-lowess))

(defmeth spin-proto :plot-controls ()
  (call-next-method)
#-macintosh (send self :install-color-symbol-buttons)
  (send self :install-toggle-linear-trend)
  (send self :install-ortho-control)
  (send self :install-scale-control)
  (send self :install-home-control)
  (send self :add-control (send spin-rock-control-proto :new 
                                (send self :locate-next-control)))
  (send self :install-extract-axis 0)
  (send self :install-extract-2d))

(defmeth scatmat-proto :plot-controls ()
  (call-next-method)
  (send self :install-color-symbol-buttons)
  (send self :install-transform-control))

(defmeth spin-proto :spin-resid-controls ()
  (call-method graph-proto :plot-controls)
#-macintosh (send self :install-color-symbol-buttons)
  (send self :install-scale-control)
  (send self :install-home-control)
  (send self :add-control (send spin-rock-control-proto :new 
                                (send self :locate-next-control)))
  (send self :install-extract-axis 0)
  (send self :install-extract-2d)
  (send self :add-control (send 3d-control-proto :new
                                (send self :locate-next-control)))
  (send self :redraw))

