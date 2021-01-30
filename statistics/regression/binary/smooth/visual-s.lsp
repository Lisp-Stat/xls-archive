(defproto binary-smooth-visual-proto
  '(col-x resp-fun-prob-plot resp-fun-logit-plot resp-fun-prob-plot-over biplot-over resp-fun-logit-plot-over resid-plot resid-plot-over univariate-plot univariate-plot-over obs-names-win col-names stats-window biplot))

(defmeth binary-smooth-visual-proto :isnew
  ()
  (send self :col-x (column-list (send *binary-model-object* :x-use)))
  (send self :col-names (send *binary-model-object* :x-use-names))
  (send self :biplot-maker)
  (send self :resp-fun-prob-plot-maker)
  (send self :resid-plot-maker)
  (send self :univariate-plot-maker)
  (send self :obs-names-window-maker)
  (send self :stats-window-maker))

(defmeth binary-smooth-visual-proto :col-x (&optional (list nil set))
"Args: (&optional list)
Sets or returns data matrix (less intercept) as list of vectors"
  (when set (setf (slot-value 'col-x) list))
  (slot-value 'col-x))

(defmeth binary-smooth-visual-proto :col-names (&optional (list nil set))
"Args: (&optional list)
Sets or returns list of independent variable names (less intercept)"
  (when set (setf (slot-value 'col-names) list))
  (slot-value 'col-names))

(defmeth binary-smooth-visual-proto :resp-fun-prob-plot
  (&optional (window nil set))
"Args: (&optional window)
Sets or returns probability response function window"
  (when set (setf (slot-value 'resp-fun-prob-plot) window))
  (slot-value 'resp-fun-prob-plot))

(defmeth binary-smooth-visual-proto :biplot
  (&optional (window nil set))
"Args: (&optional window)
Sets or returns biplot window"
  (when set (setf (slot-value 'biplot) window))
  (slot-value 'biplot))

(defmeth binary-smooth-visual-proto :biplot-over
  (&optional (overlay nil set))
"Args: (&optional overlay)
Sets or returns biplot overlay"
  (when set (setf (slot-value 'biplot-over) overlay))
  (slot-value 'biplot-over))

(defmeth binary-smooth-visual-proto :resp-fun-prob-plot-over
  (&optional (overlay nil set))
"Args: (&optional overlay)
Sets or returns probability response function overlay"
  (when set (setf (slot-value 'resp-fun-prob-plot-over) overlay))
  (slot-value 'resp-fun-prob-plot-over))

(defmeth binary-smooth-visual-proto :stats-window
  (&optional (window nil set))
"Args: (&optional window)
Sets or returns statistics window"
  (when set (setf (slot-value 'stats-window) window))
  (slot-value 'stats-window))

(defmeth binary-smooth-visual-proto :resp-fun-logit-plot
  (&optional (window nil set))
"Args: (&optional window)
Sets or returns logit response function window"
  (when set (setf (slot-value 'resp-fun-logit-plot) window))
  (slot-value 'resp-fun-logit-plot))

(defmeth binary-smooth-visual-proto :resp-fun-logit-plot-over
  (&optional (overlay nil set))
"Args: (&optional overlay)
Sets or returns logit response function window overlay"
  (when set (setf (slot-value 'resp-fun-logit-plot-over) overlay))
  (slot-value 'resp-fun-logit-plot-over))

(defmeth binary-smooth-visual-proto :resid-plot
  (&optional (window nil set))
"Args: (&optional window)
Sets or returns residual plot window"
  (when set (setf (slot-value 'resid-plot) window))
  (slot-value 'resid-plot))

(defmeth binary-smooth-visual-proto :resid-plot-over
  (&optional (overlay nil set))
"Args: (&optional overlay)
Sets or returns residual plot window overlay"
  (when set (setf (slot-value 'resid-plot-over) overlay))
  (slot-value 'resid-plot-over))

(defmeth binary-smooth-visual-proto :univariate-plot
  (&optional (window nil set))
"Args: (&optional window)
Sets or returns univariate plot window"
  (when set (setf (slot-value 'univariate-plot) window))
  (slot-value 'univariate-plot))

(defmeth binary-smooth-visual-proto :univariate-plot-over
  (&optional (overlay nil set))
"Args: (&optional overlay)
Sets or returns univariate plot window overlay"
  (when set (setf (slot-value 'univariate-plot-over) overlay))
  (slot-value 'univariate-plot-over))

(defmeth binary-smooth-visual-proto :obs-names-win
  (&optional (window nil set))
"Args: (&optional window)
Sets or returns observation names window"
  (when set (setf (slot-value 'obs-names-win) window))
  (slot-value 'obs-names-win))

(defmeth binary-smooth-visual-proto :resp-fun-prob-plot-maker
  ()
  (let* ((y (send *binary-model-object* :y))
         (sum (sum y))
         (length (length y))
         (diff (- length sum))
         (col-x (send self :col-x))
         (plot-x (select col-x (list 0 1)))
         (xs (send *binary-smooth-object* :x-estimate))
         (xs1 (select xs 0))
         (xs2 (select xs 1))
         (xx1 (select col-x 0))
         (xx2 (select col-x 1))
         (dim-vals (send *binary-smooth-object* :dim-vals))
         (line1 (select dim-vals 0))
         (line2 (select dim-vals 1))
         (rest (rest (rest dim-vals)))
         (rest (if rest (mean (append rest)) 0))
         (names (send self :col-names))
         (pred-val-p (send *binary-smooth-object*
                           :pred-val-p))
         (mat (apply #'bind-rows
                     (mapcar #'(lambda (x)
                                 (+ x line2))
                             line1)))
         (mat (+ mat rest))
         (exp (exp mat))
         (exp (/ exp (+ 1 exp)))
         (plot (spin-plot
                (list xx1 xx2 pred-val-p)
                :show t
                :size (list 210 210)
                :location (list 228 38)
                :title "Probability Function"
                :variable-labels
                (append (select names (list 0 1))
                        (list 
                         (send *binary-model-object* :y-name)))))
         (plot-over (send graph-overlay-proto :new))
         (ascent (send plot :text-ascent))
         (descent (send plot :text-descent)))
    (defmeth plot-over :location () (slot-value 'location))
    (defmeth plot-over :redraw ()
      (let* ((loc (send self :location))
             (x (first loc))
             (y (second loc))
             (box (third loc))
             (string-x (fourth loc))
             (graph (send self :graph)))
        (send graph :frame-rect x (- y box) box box)
        (send graph :draw-string "New Plot" string-x y)))
    (defmeth plot-over :do-click (x y m1 m2)
      (let* ((loc (send self :location))
             (box (third loc))
             (left (first loc))
             (top (- (second loc) box))
             (right (+ left box))
             (bottom (+ top box))
             (graph (send self :graph))
             (yy (send *binary-model-object* :y))
             (sum (sum yy))
             (length (length yy))
             (diff (- length sum)))
        (when (and (< left x right) (< top y bottom))
              (let* ((names (send *binary-smooth-visual-object* :col-names))
                     (list (first (choose-subset-dialog
                                   "New variables" names)))
                     (z (+ (send *binary-smooth-object* :resids-z)
                           (send *binary-smooth-object* :pred-val))))
                (when (equalp (length list) 2)
                      (let*
                        ((lines
                          (select (send *binary-smooth-object*
                                        :x-estimate) list))
                         (dim-vals (send *binary-smooth-object* :dim-vals))
                         (responses (select dim-vals list))
                         (rest (select dim-vals
                                       (remove-if
                                        #'(lambda (x)
                                            (member x list))
                                        (iseq (length dim-vals)))))
                         (rest (if rest (mean (append rest)) 0))
                         (points 
                          (select (send *binary-smooth-visual-object*
                                        :col-x) list))
                         (response1 (select responses 0))
                         (response2 (select responses 1))
                         (lines1 (select lines 0))
                         (lines2 (select lines 1))
                         (mat (apply #'bind-rows
                                     (mapcar #'(lambda (x)
                                                 (+ x response2))
                                             response1)))
                         (mat (+ mat rest))
                         (exp (exp mat))
                         (exp (/ exp (+ 1 exp))))
                        (send graph :start-buffering)
                        (send graph :clear)
                        (send graph :add-points 
                              (list (select points 0)
                                    (select points 1)
                                    (cond 
                                      ((and (not m1) (not m2))
                                       pred-val-p)
                                      ((and (not m1) m2)
                                       (let ((exp-z (exp z)))
                                         (/ exp-z (+ 1 exp-z))))
                                      ((and m1 (not m2))
                                       (send *binary-smooth-object* 
                                             :pred-val))
                                      ((and m1 m2) z))))
                        (send graph :variable-label (list 0 1 2)
                              (append
                               (select names list) 
                               (list
                                (send *binary-model-object* :y-name))))
                        (send graph :add-surface lines1 lines2 
                              (if m1 mat exp)) 
                        (send graph :point-color (iseq diff) 'blue)
                        (send graph :point-color (+ diff (iseq sum)) 'red)
                        (send graph :adjust-to-data)
                        (send graph :point-symbol (iseq length) 'disk)
                        (send graph :title 
                              (if m1 "Logit Function"
                                  "Probability Function"))
                        (send graph :buffer-to-screen))))
                t)))
    (send plot-over :add-slot 'location
          (list ascent (round (* 1.5 ascent))
                ascent (round (+ ascent (* 1.5 ascent)))))
    (send plot :margin 0 (round (* 1.5 (+ descent ascent))) 0 20)
    (send plot :use-color t)
    (send plot :add-overlay plot-over)
    (send plot :point-color (iseq diff) 'blue)
    (send plot :point-color (+ diff (iseq sum)) 'red)
    (send plot :reverse-colors)
    (send plot :depth-cuing nil)
    (send plot :add-surface xs1 xs2 exp)
    (send plot :adjust-to-data)
    (send plot :point-symbol (iseq length) 'disk)
    (send plot :show-window)
    (send self :resp-fun-prob-plot plot)
    (send self :resp-fun-prob-plot-over plot-over)))

(defmeth binary-smooth-visual-proto :resid-plot-maker
  ()
  (let* ((y (send *binary-model-object* :y))
         (sum (sum y))
         (length (length y))
         (diff (- length sum))
         (xx1 (select (send self :col-x) 0))
         (xx2 (send *binary-smooth-object* :resids-z))
         (plot (plot-points xx1 xx2
                            :show nil
                            :title "Residual Plot"
                            :location (list 1 268)
                            :variable-labels
                            (list (select
                                   (send self :col-names) 0)
                                  "Standardized Residual")
                            :size (list 210 210)))
         (plot-over (send graph-overlay-proto :new))
         (ascent (send plot :text-ascent))
         (descent (send plot :text-descent)))
    (send plot :abline 0 0)
    (defmeth plot-over :location () (slot-value 'location))
    (defmeth plot-over :redraw ()
      (let* ((loc (send self :location))
             (x (first loc))
             (y (second loc))
             (box (third loc))
             (string-x (fourth loc))
             (graph (send self :graph)))
        (send graph :frame-rect x (- y box) box box)
        (send graph :draw-string "New Plot" string-x y)))
    (defmeth plot-over :do-click (x y m1 m2)
      (let* ((loc (send self :location))
             (box (third loc))
             (left (first loc))
             (top (- (second loc) box))
             (right (+ left box))
             (bottom (+ top box))
             (graph (send self :graph))
             (yy (send *binary-model-object* :y))
             (sum (sum yy))
             (length (length yy))
             (diff (- length sum)))
        (when (and (< left x right) (< top y bottom))
              (let* ((names (send *binary-smooth-visual-object* :col-names))
                     (index (choose-item-dialog "New variable" names)))
                (when index
                      (let ((xxx1 (select (send *binary-smooth-object*
                                                :col-x) index))
                            (xxx2 (send *binary-smooth-object* 
                                        :resids-z)))
                        (send graph :start-buffering)
                        (send graph :clear)
                        (send graph :add-points (list xxx1 xxx2))
                        (send graph :point-color (iseq diff) 'blue)
                        (send graph :point-color (+ diff (iseq sum)) 'red)
                        (send graph :variable-label 0 (select names index))
                        (send graph :range (list 0 1)
                              (list (min xxx1) (min xxx2))
                              (list (max xxx1) (max xxx2)))
                        (send graph :abline 0 0)
                        (send graph :x-axis t t 3)
                        (send graph :y-axis t t 3)
                        (send graph :buffer-to-screen))))
              t)))
    (send plot-over :add-slot 'location
          (list ascent (round (* 1.5 ascent))
                ascent (round (+ ascent (* 1.5 ascent)))))
    (send plot :margin 0 (round (* 1.5 (+ descent ascent))) 0 0)
    (send plot :use-color t)
    (send plot :add-overlay plot-over)
    (send plot :point-color (iseq diff) 'blue)
    (send plot :point-color (+ diff (iseq sum)) 'red)
    (send plot :range (list 0 1)
          (list (min xx1) (min xx2))
          (list (max xx1) (max xx2)))
    (send plot :x-axis t t 3)
    (send plot :y-axis t t 3)
    (send plot :show-window)
    (send self :resid-plot plot)
    (send self :resid-plot-over plot-over)))

(defmeth binary-smooth-visual-proto :univariate-plot-maker
  ()
  (let* ((y (send *binary-model-object* :y))
         (sum (sum y))
         (length (length y))
         (diff (- length sum))
         (dim-vals (send *binary-smooth-object* :dim-vals))
         (out-mean (mean (rest dim-vals)))
         (xx1 (select (send self :col-x) 0))
         (xx2 (+ (select (send *binary-smooth-object* :adj-dv) 0) out-mean))
         (plot (plot-points xx1 xx2
                            :show nil
                            :title "Marginal Logits"
                            :location (list 228 268)
                            :variable-labels
                            (list (select
                                   (send self :col-names) 0)
                                  "Adjusted dependent variable")
                            :size (list 210 210)))
         (plot-over (send graph-overlay-proto :new))
         (ascent (send plot :text-ascent))
         (descent (send plot :text-descent)))
    (defmeth plot-over :location () (slot-value 'location))
    (defmeth plot-over :redraw ()
      (let* ((loc (send self :location))
             (x (select loc 0))
             (y (select loc 1))
             (box (select loc 2))
             (string-x (select loc 3))
             (graph (send self :graph)))
        (send graph :frame-rect x (- y box) box box)
        (send graph :draw-string "New Plot" string-x y)))
    (defmeth plot-over :do-click (x y m1 m2)
      (let* ((loc (send self :location))
             (box (select loc 2))
             (left (select loc 0))
             (top (- (select loc 1) box))
             (right (+ left box))
             (bottom (+ top box))
             (graph (send self :graph))
             (yy (send *binary-model-object* :y))
             (sum (sum yy))
             (length (length yy))
             (diff (- length sum)))
        (when (and (< left x right) (< top y bottom))
              (let* ((names 
                      (send *binary-smooth-visual-object* :col-names))
                     (index (choose-item-dialog "New variable" names)))
                (when index
                      (let* ((dim-vals (send *binary-smooth-object* 
                                             :dim-vals))
                             (out-mean 
                              (mean 
                               (select dim-vals
                                       (remove index (iseq (length names))))))
                             (xxx1 
                              (select 
                               (send *binary-smooth-visual-object*
                                     :col-x) index))
                             (xxx2 
                              (+ (select
                                  (send *binary-smooth-object*
                                        :adj-dv) index) out-mean))
                             (x-estimate
                              (select 
                               (send *binary-smooth-object*
                                     :x-estimate) index))
                             (dim-val 
                              (+ (select dim-vals index) out-mean))
                             (exp-dim-val (exp dim-val))
                             (exp-xxx2 (exp xxx2))
                             (exp-xxx2 (/ exp-xxx2 (+ 1 exp-xxx2))))
                        (send graph :start-buffering)
                        (send graph :clear)
                        (if m1 
                            (send graph :add-points xxx1 exp-xxx2)
                            (send graph :add-points xxx1 xxx2))
                        (if m1 
                            (send graph :add-lines
                                  (list 
                                   x-estimate
                                   (/ exp-dim-val (+ 1 exp-dim-val))))
                            (send graph :add-lines 
                              (list x-estimate dim-val)))
                        (send graph :variable-label 0 (select names index))
                        (if m1 
                            (send graph :variable-label 
                                  1 "Adjusted probability")
                            (send graph :variable-label
                                  1 "Adjusted dependent variable"))
                        (send graph :point-color (iseq diff) 'blue)
                        (send graph :point-color (+ diff (iseq sum)) 'red)
                        (if m1 
                            (send graph :range (list 0 1)
                                  (list (min xxx1) (min exp-xxx2))
                                  (list (max xxx1) (max exp-xxx2)))
                            (send graph :range (list 0 1)
                                  (list (min xxx1) (min xxx2))
                                  (list (max xxx1) (max xxx2))))
                        (send graph :x-axis t t 3)
                        (send graph :y-axis t t 3)
                        (if m1
                            (send graph :title "Marginal Probabilities")
                            (send graph :title "Marginal Logits"))
                        (send graph :buffer-to-screen))))
              t)))
    (send plot-over :add-slot 'location
          (list ascent (round (* 1.5 ascent))
                ascent (round (+ ascent (* 1.5 ascent)))))
    (send plot :margin 0 (round (* 1.5 (+ descent ascent))) 0 0)
    (send plot :add-lines 
          (select (send *binary-smooth-object* :x-estimate) 0)
          (+ (select dim-vals 0) out-mean))
    (send plot :use-color t)
    (send plot :range (list 0 1)
          (list (min xx1) (min xx2))
          (list (max xx1) (max xx2)))
    (send plot :add-overlay plot-over)
    (send plot :point-color (iseq diff) 'blue)
    (send plot :point-color (+ diff (iseq sum)) 'red)
    (send plot :x-axis t t 3)
    (send plot :y-axis t t 3)
    (send plot :show-window)
    (send self :univariate-plot plot)
    (send self :univariate-plot-over plot-over)))

(defmeth binary-smooth-visual-proto :obs-names-window-maker ()
  (let* ((y (send *binary-model-object* :y))
         (sum (sum y))
         (length (length y))
         (diff (- length sum))
         (window (name-list (send *binary-model-object* :obs-names) 
                             :show nil)))
    (send window :size 184 239)
    (send window :location 455 18)
    (send window :use-color t)
    (send window :point-color (iseq diff) 'blue)
    (send window :point-color (+ diff (iseq sum)) 'red)
    (send window :title "Observations")
    (send window :show-window)
    (send self :obs-names-win window)))

(defmeth binary-smooth-visual-proto :stats-window-maker ()
  (let* ((window (send graph-window-proto :new :show nil))
         (names (send *binary-model-object* :x-use-names))
         (estimates (mapcar #'(lambda (x) (- x (mean x)))
                            (send *binary-smooth-object* :dim-smooth)))
         (pred-val (send *binary-smooth-object* :pred-val))
         (pred-val (- pred-val (mean pred-val)))
         (ssq-ests (mapcar #'(lambda (x) (sum (^ x 2))) estimates))
         (ssq-pv (sum (^ pred-val 2)))
         (pct (* 100 (mapcar #'(lambda (x) (/ x ssq-pv)) ssq-ests))))
    (defmeth window :redraw ()
      (let* ((ta (send window :text-ascent))
             (twp (send window :text-width "Parameter"))
             (twe (send window :text-width "SSQ")))
        (send window :draw-string "Parameter" 2 10)
        (send window :draw-string "SSQ" (+ twp 22) 10)
        (send window :draw-string "% Total" (+ twe twp 50) 10)
        (dotimes 
         (i (length names))
         (send window :draw-string
               (select names i)
               2 (+ (* ta i) ta 15 (* 5 i)))
         (send window :draw-string
               (format nil "~6,4f" (select ssq-ests i))
               (+ twe 40)
               (+ (* ta i) ta 15 (* 5 i)))
         (send window :draw-string
               (format nil "~6,4f"  (select pct i))
               (+ twe twp 50)
               (+ (* ta i) ta 15 (* 5 i))))
        (send window :draw-string
              (format nil "Log Likelihood   ~8,6f" 
                      (send *binary-smooth-object* :likelihood))
              2
              (+ (* ta (+ (length names) 2))
                 15 (* 5 (+ (length names) 1))))))
    (send window :size 170 182)
    (send window :location 455 297)
    (send window :title "Statistics")
    (send window :show-window)
    (send self :stats-window window)))

(defmeth binary-smooth-visual-proto :biplot-maker
  ()
  (let* ((biplot-x (send self :col-x))
         (y (send *binary-model-object* :y))
         (sum (sum y))
         (length (length y))
         (diff (- length sum))
         (xx1 (select biplot-x 0))
         (xx2 (select biplot-x 1))
         (names (send self :col-names))
         (plot (plot-points xx1 xx2
                            :show nil
                            :title "Scatterplot"
                            :location (list 1 38)
                            :variable-labels
                            (select names (list 0 1))
                            :size (list 210 210)))
         (plot-over (send graph-overlay-proto :new))
         (ascent (send plot :text-ascent))
         (descent (send plot :text-descent)))
    (defmeth plot-over :location () (slot-value 'location))
    (defmeth plot-over :redraw ()
      (let* ((loc (send self :location))
             (x (first loc))
             (y (second loc))
             (box (third loc))
             (string-x (fourth loc))
             (graph (send self :graph)))
        (send graph :frame-rect x (- y box) box box)
        (send graph :draw-string "New Plot" string-x y)))
    (defmeth plot-over :do-click (x y m1 m2)
      (let* ((loc (send self :location))
             (box (select loc 2))
             (left (select loc 0))
             (top (- (select loc 1) box))
             (right (+ left box))
             (bottom (+ top box))
             (graph (send self :graph))
             (yy (send *binary-model-object* :y))
             (sum (sum yy))
             (length (length yy))
             (diff (- length sum)))
        (when (and (< left x right) (< top y bottom))
              (let* ((names (send
                             *binary-smooth-visual-object*
                             :col-names))
                     (index (first
                             (choose-subset-dialog
                              "New variables" names))))
                (when
                 (equalp (length index) 2)
                 (let* ((ind1 (select index 0))
                        (ind2 (select index 1))
                        (biplot-x (send 
                                   *binary-smooth-visual-object*
                                   :col-x))
                        (xxx1 (select biplot-x ind1))
                        (xxx2 (select biplot-x ind2)))
                   (send graph :start-buffering)
                   (send graph :clear)
                   (send graph :add-points 
                         (list xxx1 xxx2))
                   (send graph :range
                         (list 0 1)
                         (list (min xxx1) (min xxx2))
                         (list (max xxx1) (max xxx2)))
                   (send graph :x-axis t t 3)
                   (send graph :y-axis t t 3)
                   (send graph :variable-label (list 0 1) 
                         (select names index))
                   (send graph :point-color (iseq diff) 'blue)
                   (send graph :point-color (+ diff (iseq sum)) 'red)
                   (send graph :redraw)
                   (send graph :buffer-to-screen))))
              t)))
    (send plot-over :add-slot 'location
          (list ascent (round (* 1.5 ascent))
                ascent (round (+ ascent (* 1.5 ascent)))))
    (send plot :margin 0 (round (* 1.5 (+ descent ascent))) 0 0)
    (send plot :use-color t)
    (send plot :add-overlay plot-over)
    (send plot :point-color (iseq diff) 'blue)
    (send plot :point-color (+ diff (iseq sum)) 'red)
    (send plot :range
          (list 0 1)
          (list (min xx1) (min xx2))
          (list (max xx1) (max xx2)))
    (send plot :x-axis t t 3)
    (send plot :y-axis t t 3)
    (send plot :show-window)
    (send self :biplot plot)
    (send self :biplot-over plot-over)))
