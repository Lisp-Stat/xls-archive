(defproto binary-parametric-visual-proto
  '(biplot col-x resp-fun-plot resp-fun-plot-over resid-plot infl-plot infl-plot-over biplot-over col-names resid-plot-over obs-names-win stats-window n-ivs))

(defmeth binary-parametric-visual-proto :isnew
  (int)
  (send self :col-x (column-list (send *binary-model-object* :x-use)))
  (send self :col-names (send *binary-model-object* :x-use-names))
  (send self :n-ivs (length (send *binary-model-object* :x-selector)))
  (send self :biplot-maker)
  (send self :resp-fun-plot-maker (+ (list 0 1) int))
  (send self :infl-plot-maker
        (send *binary-parametric-object* :hj)
        (send *binary-parametric-object* :delta-beta))
  (send self :resid-plot-maker)
  (send self :obs-names-window-maker)
  (send self :stats-window-maker))

(defmeth binary-parametric-visual-proto :col-x (&optional (list nil set))
"Args: (&optional list)
Sets or returns data matrix (less intercept) as list of vectors"
  (when set (setf (slot-value 'col-x) list))
  (slot-value 'col-x))

(defmeth binary-parametric-visual-proto :n-ivs (&optional (number nil set))
"Args: (&optional matrix)
Sets or returns number of independent variables"
  (when set (setf (slot-value 'n-ivs) number))
  (slot-value 'n-ivs))

(defmeth binary-parametric-visual-proto :col-names (&optional (list nil set))
"Args: (&optional list)
Sets or returns variable names (less intercept)"
  (when set (setf (slot-value 'col-names) list))
  (slot-value 'col-names))

(defmeth binary-parametric-visual-proto :biplot (&optional (plot nil set))
"Args: (&optional plot)
Sets or returns biplot"
  (when set (setf (slot-value 'biplot) plot))
  (slot-value 'biplot))

(defmeth binary-parametric-visual-proto :resp-fun-plot
  (&optional (plot nil set))
"Args: (&optional plot)
Sets or returns response function plot"
  (when set (setf (slot-value 'resp-fun-plot) plot))
  (slot-value 'resp-fun-plot))

(defmeth binary-parametric-visual-proto :infl-plot (&optional (plot nil set))
"Args: (&optional plot)
Sets or returns influence plot"
  (when set (setf (slot-value 'infl-plot) plot))
  (slot-value 'infl-plot))

(defmeth binary-parametric-visual-proto :resid-plot (&optional (plot nil set))
"Args: (&optional plot)
Sets or returns residual plot"
  (when set (setf (slot-value 'resid-plot) plot))
  (slot-value 'resid-plot))

(defmeth binary-parametric-visual-proto :obs-names-win
  (&optional (window nil set))
"Args: (&optional window)
Sets or returns observation names window"
  (when set (setf (slot-value 'obs-names-win) window))
  (slot-value 'obs-names-win))

(defmeth binary-parametric-visual-proto :stats-window
  (&optional (window nil set))
"Args: (&optional window)
Sets or returns statistics window"
  (when set (setf (slot-value 'stats-window) window))
  (slot-value 'stats-window))

(defmeth binary-parametric-visual-proto :resp-fun-plot-over (&optional (overlay nil set))
"Args: (&optional overlay)
Sets or returns response function plot overlay"
  (when set (setf (slot-value 'resp-fun-plot-over) overlay))
  (slot-value 'resp-fun-plot-over))

(defmeth binary-parametric-visual-proto :resid-plot-over
  (&optional (overlay nil set))
"Args: (&optional overlay)
Sets or returns residual plot overlay"
  (when set (setf (slot-value 'resid-plot-over) overlay))
  (slot-value 'resid-plot-over))

(defmeth binary-parametric-visual-proto :biplot-over
  (&optional (overlay nil set))
"Args: (&optional overlay)
Sets or returns biplot overlay"
  (when set (setf (slot-value 'biplot-over) overlay))
  (slot-value 'biplot-over))

(defmeth binary-parametric-visual-proto :infl-plot-over (&optional (overlay nil set))
"Args: (&optional overlay)
Sets or returns influence plot overlay"
  (when set (setf (slot-value 'infl-plot-over) overlay))
  (slot-value 'infl-plot-over))

(defmeth binary-parametric-visual-proto :infl-replot ()
  (let* ((var-names (send *binary-model-object* :bi-resid-x-names))
         (nvars (length var-names))
         (x-plot nil)
         (y-plot nil)
         (x-name nil)
         (y-name nil)
         (x (choose-item-dialog
             "x axis"
             (append var-names
                     (list
                      "hat matrix diagonal"
                      "predicted response probability")))))
    (cond
      ((member x (iseq nvars))
       (setf x-plot (select (send *binary-model-object* :bi-resid-x) x))
       (setf x-name (list (select var-names x)))
       (let ((y (choose-item-dialog
                 "y axis"
                 (list "Df beta" "Delta beta" "C-bar"
                       "Delta deviance" "Delta chi-square"))))
         (case y
           (0 (setf y-plot
                    (select
                     (send *binary-parametric-object* :dfbeta)
                     x))
              (setf y-name (list "Df beta")))
           (1 (setf y-plot
                    (send *binary-parametric-object* :delta-beta))
              (setf y-name (list "Delta beta")))
           (2 (setf y-plot
                    (send *binary-parametric-object* :c-bar))
              (setf y-name (list "C bar")))
           (3 (setf y-plot
                    (send *binary-parametric-object* :delta-d))
              (setf y-name (list "Delta deviance")))
           (4 (setf y-plot
                    (send *binary-parametric-object* :delta-x-sq))
              (setf y-name (list "Delta chi-square"))))))
      ((eq x nvars)
       (setf x-plot (send *binary-parametric-object* :hj))
       (setf x-name (list "Hat matrix diagonal"))
       (let ((y (choose-item-dialog
                 "y axis"
                 (list "Delta chi-square" "Delta deviance" "Delta beta"))))
         (case y
           (0 (setf y-plot 
                    (send *binary-parametric-object* :delta-x-sq))
              (setf y-name (list "Delta chi-square")))
           (1 (setf y-plot 
                    (send *binary-parametric-object* :delta-d))
              (setf y-name (list "Delta deviance")))
           (2 (setf y-plot
                    (send *binary-parametric-object* :delta-beta))
              (setf y-name (list "Delta beta"))))))
      ((eq x (+ 1 nvars))
       (setf x-plot (send *binary-parametric-object* :pred-val-p))
       (setf x-name (list "Predicted probability"))
       (let ((y (choose-item-dialog
                 "y axis"
                 (list "Delta chi-square" "Delta deviance" "Delta beta"))))
         (case y
           (0 (setf y-plot
                    (send *binary-parametric-object* :delta-x-sq))
              (setf y-name (list "Delta chi-square")))
           (1 (setf y-plot
                    (send *binary-parametric-object* :delta-d))
              (setf y-name (list "Delta deviance")))
           (2 (setf y-plot
                    (send *binary-parametric-object* :delta-beta))
              (setf y-name (list "Delta beta")))))))
    (list (list x-plot y-plot) (append x-name y-name))))

(defmeth binary-parametric-visual-proto :resp-fun-plot-maker (list)
  (let* ((y (send *binary-model-object* :y))
         (sum (sum y))
         (length (length y))
         (diff (- length sum))
         (estimates (send *binary-parametric-object* :estimates))
         (full-x (send *binary-model-object* :full-x))
         (x-selector (send *binary-model-object* :x-selector))
         (linked-vars (send *binary-model-object* :linked-vars))
         (ind1 (select x-selector (select list 0)))
         (ind2 (select x-selector (select list 1)))
         (link1 (remove-if-not 
                 #'(lambda (x)
                     (equalp (select x 0) ind1)) 
                 linked-vars))
         (link2 (remove-if-not
                 #'(lambda (x)
                     (equalp (select x 0) ind2))
                 linked-vars))
         (link-out (remove-if
                    #'(lambda (x)
                        (member (select x 0) (list ind1 ind2)))
                    linked-vars))
         (indices-full (append x-selector (mapcar #'second linked-vars)))
         (estimates (select estimates (order indices-full)))
         (indices-in (append (list ind1 ind2) 
                             (mapcar #'second link1)
                             (mapcar #'second link2)))
         (indices-out (remove-if #'(lambda (x) (member x indices-in))
                                 indices-full))
         (rest (if indices-out
                   (sum (* (select estimates 
                                   (mapcar #'(lambda (x)
                                               (position x indices-full))
                                           indices-out))
                           (mapcar #'mean (select full-x indices-out))))
                   0))
         (x1 (select full-x ind1))
         (x2 (select full-x ind2))
         (def (def seq11 (+ (min x1) (* (rseq 0 1 12)
                                        (- (max x1) (min x1))))))
         (def (def seq21 (+ (min x2) (* (rseq 0 1 12)
                                        (- (max x2) (min x2))))))
         (seq12
          (if 
           link1
           (+ 
            (apply 
             #'+ 
             (* (mapcar
                 #'(lambda (link-elm) 
                     (select estimates 
                             (position (select link-elm 1)
                                       indices-full)))
                 link1)
                (mapcar
                 #'(lambda (link-elm)
                     (eval 
                      (subst 'seq11 'x
                             (select link-elm 2))))
                 link1)))
            (* seq11 (select estimates (position ind1 indices-full))))
           (* seq11 (select estimates (position ind1 indices-full)))))
         (seq22
          (if 
           link2
           (+ 
            (apply 
             #'+ 
             (* (mapcar
                 #'(lambda (link-elm) 
                     (select estimates 
                             (position (select link-elm 1)
                                       indices-full)))
                 link2)
                (mapcar
                 #'(lambda (link-elm)
                     (eval 
                      (subst 'seq21 'x
                             (select link-elm 2))))
                 link2)))
            (* seq21 (select estimates (position ind2 indices-full))))
           (* seq21 (select estimates (position ind2 indices-full)))))
         (grid (apply #'bind-rows
                      (mapcar #'(lambda (x) (+ x seq22)) seq12)))
         (grid (+ grid rest))
         (exp (exp grid))
         (plot (spin-plot (list x1 x2 
                                (send *binary-parametric-object* 
                                      :pred-val-p))
                          :show t
                          :size (list 210 210)
                          :location (list 228 38)
                          :title "Probability Function"
                          :variable-labels
                          (append (select (send *binary-model-object*
                                               :full-x-names) 
                                          (list ind1 ind2))
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
        (when
         (and (< left x right) (< top y bottom))
         (let
           ((list 
             (first
              (choose-subset-dialog
               "New variables" 
               (send *binary-model-object* :x-use-names)))))
           (when
            (and list (= 2 (length list)))
            (let* ((list (+ (send *binary-model-object* :int)
                            list))
                   (y (send *binary-model-object* :y))
                   (sum (sum y))
                   (length (length y))
                   (diff (- length sum))
                   (estimates (send 
                               *binary-parametric-object* 
                                         :estimates))
                   (full-x (send *binary-model-object* :full-x))
                   (x-selector (send *binary-model-object* :x-selector))
                   (linked-vars (send *binary-model-object* :linked-vars))
                   (ind1 (select x-selector (select list 0)))
                   (ind2 (select x-selector (select list 1)))
                   (link1 (remove-if-not 
                           #'(lambda (x)
                               (equalp (select x 0) ind1)) 
                           linked-vars))
                   (link2 (remove-if-not
                           #'(lambda (x)
                               (equalp (select x 0) ind2))
                           linked-vars))
                   (link-out (remove-if
                              #'(lambda (x)
                                  (member (select x 0) (list ind1 ind2)))
                              linked-vars))
                   (indices-full (append x-selector
                                         (mapcar #'second linked-vars)))
                   (estimates (select estimates (order indices-full)))
                   (indices-in (append (list ind1 ind2) 
                                       (mapcar #'second link1)
                                       (mapcar #'second link2)))
                   (indices-out (remove-if 
                                 #'(lambda (x) (member x indices-in))
                                 indices-full))
                   (rest (if indices-out
                             (sum 
                              (* (select estimates 
                                         (mapcar
                                          #'(lambda (x)
                                              (position x indices-full))
                                          indices-out))
                                 (mapcar 
                                  #'mean (select full-x indices-out))))
                             0))
                   (x1 (select full-x ind1))
                   (x2 (select full-x ind2))
                   (def (def seq11 (+ (min x1)
                                      (* (rseq 0 1 12)
                                         (- (max x1) (min x1))))))
                   (def (def seq21 (+ (min x2)
                                      (* (rseq 0 1 12)
                                         (- (max x2) (min x2))))))
                   (seq12
                    (if 
                     link1
                     (+ 
                      (apply 
                       #'+ 
                       (* (mapcar
                           #'(lambda (link-elm) 
                               (select estimates 
                                       (position (select link-elm 1)
                                                 indices-full)))
                           link1)
                          (mapcar
                           #'(lambda (link-elm)
                               (eval 
                                (subst 'seq11 'x
                                       (select link-elm 2))))
                           link1)))
                      (* seq11 (select estimates
                                       (position ind1 indices-full))))
                     (* seq11 (select estimates 
                                      (position ind1 indices-full)))))
                   (seq22
                    (if 
                     link2
                     (+ 
                      (apply 
                       #'+ 
                       (* (mapcar
                           #'(lambda (link-elm) 
                               (select estimates 
                                       (position (select link-elm 1)
                                                 indices-full)))
                           link2)
                          (mapcar
                           #'(lambda (link-elm)
                               (eval 
                                (subst 'seq21 'x
                                       (select link-elm 2))))
                           link2)))
                      (* seq21 (select estimates 
                                       (position ind2 indices-full))))
                     (* seq21 (select 
                               estimates 
                               (position ind2 indices-full)))))
                   (grid (apply
                             #'bind-rows
                             (mapcar #'(lambda (x) (+ x seq22)) seq12)))
                   (grid (+ grid rest))
                   (exp (exp grid)))
              (send graph :start-buffering)
              (send graph :clear)
              (send graph :add-points 
                    (list x1 x2 
                          (cond
                            ((and m1 (not m2))
                             (send *binary-parametric-object*
                                    :pred-val))
                            ((and (not m1) (not m2))
                             (send *binary-parametric-object* 
                                   :pred-val-p))
                            ((and  m1 m2)
                             (send *binary-parametric-object* :z))
                            ((and (not m1) m2
                               (let ((exp-z
                                      (exp
                                       (send *binary-parametric-object* :z))))
                                 (/ exp-z (+ 1 exp-z))))))))                                             
              (send graph :variable-label (list 0 1 2)
                    (append
                     (select
                      (send *binary-model-object*
                            :full-x-names)
                      (list ind1 ind2))
                     (list
                      (send *binary-model-object* :y-name))))
              (send graph :point-color (iseq diff) 'blue)
              (send graph :point-color (+ diff (iseq sum)) 'red)
              (send graph 
                    :add-surface seq11 seq21 
                    (if m1 grid
                        (/ exp (+ 1 exp)))
                    :spline 1)
              (send graph :title 
                    (if m1 "Logit Function"
                        "Probability Function"))
              (send graph :adjust-to-data)
              (send graph :buffer-to-screen)
              (undef (list 'seq11 'seq21))))))))
    (send plot :adjust-to-data)
    (send plot :show-window)
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
    (send plot :add-surface seq11 seq21 (/ exp (+ 1 exp)) :spline 1)
    (send plot :adjust-to-data)
    (send plot :point-symbol (iseq length) 'disk)
    (send plot :show-window)
    (send self :resp-fun-plot plot)
    (send self :resp-fun-plot-over plot-over)
    (undef (list 'seq11 'seq21))))