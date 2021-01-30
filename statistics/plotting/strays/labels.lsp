;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Program to display labels on plots inheriting from the graph-proto
;;;;  prototype.  Labels can be inserted, removed, aligned, and dragged
;;;;  by clicking in the desired plots.  Calls to the function should be 
;;;;  of the form:
;;;;  (make-labels (list plot1 plot2 ... plotn)).  Plots supported are
;;;;  all scatterplots, histograms, and spin-plots.  These labels will
;;;;  be saved and printed along with the plot image.
;;;;
;;;;      Jason Bond (jbond@laplace.stat.ucla.edu)  3/30/95
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 (send graph-proto :add-slot 'plots)
 (send graph-proto :add-slot 'plots-with-labels)
 (send graph-proto :add-slot 'label-coordinates)
 (send graph-proto :add-slot 'plot-labels)
 (send graph-proto :add-slot 'horiz-vert)
 (send graph-proto :add-slot 'vert)
 (send graph-proto :add-slot 'horiz)
 (send graph-proto :add-slot 'c-horiz-vert 0)
 (send graph-proto :add-slot 'c-vert 0)
 (send graph-proto :add-slot 'c-horiz 0)
 (send graph-proto :add-slot 'horiz-align t)
 (send graph-proto :add-slot 'box-coords)

(defmeth graph-proto :add-labels (plot labels coords)
 (send graph-proto :plots-with-labels  
    (append (send graph-proto :plots-with-labels) (list plot)))
 (send graph-proto :label-coordinates 
    (append (send graph-proto :label-coordinates) (list coords)))
 (send graph-proto :plot-labels
    (append (send graph-proto :plot-labels) (list labels)))
 (send graph-proto :horiz-vert
    (append (send graph-proto :horiz-vert) (list (send self :c-horiz-vert))))
 (send graph-proto :horiz
    (append (send graph-proto :horiz) (list (send self :c-horiz))))
 (send graph-proto :vert
    (append (send graph-proto :vert) (list (send self :c-vert))))
)

(defmeth graph-proto :close ()
 (call-next-method)
   (let* (
          (vars (variables))
          (varplot (first (which (mapcar #'(lambda (x) (equalp x self))
                       (mapcar #'symbol-value vars)))))
          (plots (send graph-proto :plots-with-labels))
         )
    (send graph-proto :plots (remove self (send graph-proto :plots)))
    (undef (elt vars varplot))
    (when plots
     (let* (
            (plotposn (which (mapcar #'(lambda (x) (equalp x self)) plots)))
            (coords (send graph-proto :label-coordinates))
            (labels (send graph-proto :plot-labels))
            (h-v (send graph-proto :horiz-vert))
            (h (send graph-proto :horiz))
            (v (send graph-proto :vert))
            (seq (set-difference (iseq (length plots)) 
                     (if (listp plotposn) plotposn (list plotposn))))
           )
      (if (> (length seq) 0) (setf seq (sort-data seq)))
      (send graph-proto :plots-with-labels (select plots seq))
      (send graph-proto :label-coordinates (select coords seq))
      (send graph-proto :plot-labels (select labels seq))
      (send graph-proto :horiz-vert (select h-v seq))
      (send graph-proto :horiz (select h seq))
      (send graph-proto :vert (select v seq))
 ))))

(defmeth spin-proto :redraw ()
(call-next-method)
(when (send graph-proto :plots-with-labels)
 (let* (
        (plots (send graph-proto :plots-with-labels))
        (inds (which (mapcar #'(lambda (x) (equalp self x)) plots)))
        (v-h (select (send graph-proto :horiz-vert) inds))
        (horiz (select (send graph-proto :horiz) inds))
        (vert (select (send graph-proto :vert) inds))
        (method (mapcar #'(lambda (x)
                            (if (= x 0) :draw-text :draw-text-up)) v-h))
        (coords (select (send graph-proto :label-coordinates) inds))
        (labels (select (send graph-proto :plot-labels) inds))
       )
   (mapcar #'(lambda (x y z h v) (send self z x (first y) (second y) h v))
       labels coords method horiz vert))))


(defmeth scatterplot-proto :redraw ()
(call-next-method)
(when (send graph-proto :plots-with-labels)
 (let* (
        (plots (send graph-proto :plots-with-labels))
        (inds (which (mapcar #'(lambda (x) (equalp self x)) plots)))
        (v-h (select (send graph-proto :horiz-vert) inds))
        (horiz (select (send graph-proto :horiz) inds))
        (vert (select (send graph-proto :vert) inds))
        (method (mapcar #'(lambda (x)
                            (if (= x 0) :draw-text :draw-text-up)) v-h))
        (coords (select (send graph-proto :label-coordinates) inds))
        (labels (select (send graph-proto :plot-labels) inds))
       )
   (mapcar #'(lambda (x y z h v) (send self z x (first y) (second y) h v))
       labels coords method horiz vert))))

(defmeth histogram-proto :redraw ()
(call-next-method)
(when (send graph-proto :plots-with-labels)
 (let* (
        (plots (send graph-proto :plots-with-labels))
        (inds (which (mapcar #'(lambda (x) (equalp self x)) plots)))
        (v-h (select (send graph-proto :horiz-vert) inds))
        (horiz (select (send graph-proto :horiz) inds))
        (vert (select (send graph-proto :vert) inds))
        (method (mapcar #'(lambda (x)
                            (if (= x 0) :draw-text :draw-text-up)) v-h))
        (coords (select (send graph-proto :label-coordinates) inds))
        (labels (select (send graph-proto :plot-labels) inds))
       )
   (mapcar #'(lambda (x y z h v) (send self z x (first y) (second y) h v))
       labels coords method horiz vert))))



(defmeth graph-proto :remove-label (plot x y)
   (let* (
          (plots (send graph-proto :plots-with-labels))
          (removeplot (if plots
                       (which (mapcar #'(lambda (i) (equalp i plot)) plots))
                       nil))
          (coords (send graph-proto :label-coordinates))
          (labels (send graph-proto :plot-labels))
          (h-v (send self :horiz-vert))
          (h (send self :horiz))
          (v (send self :vert))
         )
    (when removeplot
     (let* (
            (xremove (which (mapcar #'(lambda (i) (< (min x) i (max x)))
                             (mapcar #'first coords))))
            (yremove (which (mapcar #'(lambda (i) (< (min y) i (max y)))
                             (mapcar #'second coords))))
            (to-remove (intersection (intersection xremove yremove) 
                                     removeplot))
           )
       (when to-remove
         (let ((seq (set-difference (iseq (length plots))
                        (if (listp to-remove) to-remove (list to-remove)))))
          (send graph-proto :horiz-vert (select h-v seq))
          (send graph-proto :horiz (select h seq))
          (send graph-proto :vert (select v seq))
          (send graph-proto :plots-with-labels (select plots seq))
          (send graph-proto :label-coordinates (select coords seq))
          (send graph-proto :plot-labels (select labels seq))))))))



(defmeth graph-proto :align (plot x y)
   (let* (
          (plots (send graph-proto :plots-with-labels))
          (alignplot (if plots
                      (which (mapcar #'(lambda (i) (equalp i plot)) plots))
                      nil))
          (coords (send graph-proto :label-coordinates))
          (horiz-align (send graph-proto :horiz-align))
         )
     (when alignplot
     (let* (
            (xalign (which (mapcar #'(lambda (i) (< (min x) i (max x)))
                             (mapcar #'first coords))))
            (yalign (which (mapcar #'(lambda (i) (< (min y) i (max y)))
                             (mapcar #'second coords))))
            (to-align (intersection (intersection xalign yalign) 
                                     alignplot))
           )
       (when to-align
          (cond (horiz-align
                 (let* (
                        (newcoords (select coords to-align))
                        (y-val (floor (mean (mapcar #'second newcoords))))
                       )
                   (mapcar #'(lambda (y) (setf (second y) y-val)) newcoords)
                   (setf (select coords to-align) newcoords)
                   (send graph-proto :label-coordinates coords)))
                (t 
                 (let* (
                        (newcoords (select coords to-align))
                        (x-val (floor (mean (mapcar #'first newcoords))))
                       )
                   (mapcar #'(lambda (x) (setf (first x) x-val)) newcoords)
                   (setf (select coords to-align) newcoords)
                   (send graph-proto :label-coordinates coords)))))))))
              

(defmeth graph-proto :drag-labels (x y m1 m2)
 (let* (
        (allplots (send graph-proto :plots))
        (plots (send graph-proto :plots-with-labels))
        (dragplot (if plots
                    (which (mapcar #'(lambda (i) (equalp i self)) plots))
                    nil))
        (coords (send graph-proto :label-coordinates))
        (b-c (send self :box-coords))
        (good-click (if (and (< (min (first b-c)) x 
                                (max (first b-c)))
                             (< (min (second b-c)) y
                                (max (second b-c)))) t nil))
       )
    (when (not dragplot) 
                         (send self :redraw)
                         (mapcar #'(lambda (x) 
                           (send x :mouse-mode 'get-drag-labels)) allplots))
    (when (and dragplot good-click)
     (let* (
            (xdrag (which (mapcar #'(lambda (i) 
                             (< (min (first b-c)) i (max (first b-c))))
                             (mapcar #'first coords))))
            (ydrag (which (mapcar #'(lambda (i) 
                             (< (min (second b-c)) i (max (second b-c))))
                             (mapcar #'second coords))))
            (to-drag (intersection (intersection xdrag ydrag) dragplot))
           )
     (when (not to-drag)
                     (send self :redraw)
                     (mapcar #'(lambda (x) 
                      (send x :mouse-mode 'get-drag-labels)) allplots))
     (when to-drag
      (let* ( 
            (v-h (select (send graph-proto :horiz-vert) to-drag))
            (horiz (select (send graph-proto :horiz) to-drag))
            (vert (select (send graph-proto :vert) to-drag))
            (method (mapcar #'(lambda (x)
                               (if (= x 0) :draw-text :draw-text-up)) v-h))
            (crds (select coords to-drag))
            (labels (select (send graph-proto :plot-labels) to-drag))
            (xdiff (apply #'- (first b-c)))
            (ydiff (apply #'- (second b-c)))
            (lx (first b-c))
            (ly (second b-c))
            (ox x)
            (oy y)
            (n-c nil)
            )
        (send self :while-button-down #'(lambda (x1 y1)
                    (send self :draw-mode 'xor)
                    (let (
                          (xmove (- x1 ox))
                          (ymove (- y1 oy))
                         )
                       (mapcar #'(lambda (x y z h v) 
                          (send self z x (first y) (second y) h v))
                           labels crds
                           method horiz vert)
                       (setf crds (mapcar #'(lambda (x) 
                                (setf x (+ x (list xmove ymove)))) crds))
                       (send self :draw-line (first lx) (first ly) 
                                             (first lx) (second ly))
                       (send self :draw-line (first lx) (second ly)
                                             (second lx) (second ly))
                       (send self :draw-line (second lx) (second ly)
                                             (second lx) (first ly))
                       (send self :draw-line (second lx) (first ly)
                                             (first lx) (first ly))

                       (send self :draw-line (+ (first lx) xmove)
                                             (+ (first ly) ymove)
                                             (+ (first lx) xmove)
                                             (+ (second ly) ymove))
                       (send self :draw-line (+ (first lx) xmove)
                                             (+ (second ly) ymove)
                                             (+ (second lx) xmove)
                                             (+ (second ly) ymove))
                       (send self :draw-line (+ (second lx) xmove)
                                             (+ (second ly) ymove)
                                             (+ (second lx) xmove)
                                             (+ (first ly) ymove))
                       (send self :draw-line (+ (second lx) xmove)
                                              (+ (first ly) ymove)
                                              (+ (first lx) xmove)
                                              (+ (first ly) ymove))
                       (mapcar #'(lambda (x y z h v) 
                          (send self z x (first y) (second y) h v))
                           labels crds 
                           method horiz vert)
                       (setf ox x1)
                       (setf oy y1)
                       (setf lx (+ lx xmove))
                       (setf ly (+ ly ymove))
                       (setf n-c (list (min lx) (min ly))))))
       (unless n-c (setf n-c (list (min lx) (min ly))))
       (send self :draw-mode 'normal)
       (let (         
            (c-d (cond ((and (< xdiff 0) (< ydiff 0))
                         (- (select n-c (list 0 1)) (mapcar #'first b-c)))
                       ((and (> xdiff 0) (< ydiff 0))
                         (- (select n-c (list 0 1)) 
                            (list (second (first b-c)) (first (second b-c)))))
                       ((and (> xdiff 0) (> ydiff 0))
                         (- (select n-c (list 0 1)) (mapcar #'second b-c)))
                       (t (- (select n-c (list 0 1)) 
                             (list (first (first b-c)) 
                                   (second (second b-c)))))))
             (newcoords (select coords to-drag))
            )
              (setf newcoords (mapcar #'(lambda (y) (setf y (+ y c-d))) 
                                newcoords))
              (setf (select coords to-drag) newcoords)
              (send graph-proto :label-coordinates coords)))
         (mapcar #'(lambda (x) (send x :mouse-mode 'get-drag-labels)) 
            allplots)))
         (send self :redraw))))


       
(defmeth graph-proto :draw-label (x y m1 m2)
(let* ((loc (send self :location))
       (newx (+ x (first loc) (- 80)))
       (newy (+ y (second loc) (- 80)))
       (put-string (get-string-dialog "Enter a Label:" 
                      :location (list newx newy))))
 (if put-string (send self :add-labels self put-string (list x y))))
  (send self :redraw))

(defmeth graph-proto :erase-label (x y m1 m2)
 (let ((coords (send self :draw-box x y)))
   (send self :remove-label self 
           (list x (first coords)) (list y (second coords)))
   (send self :redraw)))

(defmeth graph-proto :align-labels (x y m1 m2)
 (let ((coords (send self :draw-box x y))
       (horiz (choose-item-dialog "Align:" 
                 (list "Horizontally" "Vertically"))))
   (when horiz (send graph-proto :horiz-align (if (= horiz 0) t nil))
               (send self :align self
                 (list x (first coords)) (list y (second coords))))
   (send self :redraw)))

(defmeth graph-proto :get-drag-labels (x y m1 m2)
  (let ((coords (send self :draw-box x y))
        (plots (send graph-proto :plots)))
    (send graph-proto :box-coords (list (list x (first coords))
                                        (list y (second coords))))
    (mapcar #'(lambda (x) (send x :mouse-mode 'drag-labels)) plots)))

(defmeth graph-proto :draw-box (x y)
 (let ((newx x)
       (newy y))
 (send self :draw-mode 'xor)
 (send self :while-button-down #'(lambda (x1 y1) 
                      (send self :draw-line x y x y1)
                      (send self :draw-line x y x1 y)
                      (send self :draw-line x y1 x1 y1)
                      (send self :draw-line x1 y1 x1 y)
                      (send self :draw-line x y x newy)
                      (send self :draw-line x y newx y) 
                      (send self :draw-line x newy newx newy)
                      (send self :draw-line newx newy newx y)
                      (setf newx x1)
                      (setf newy y1)))
 (send self :draw-mode 'normal)
 (list newx newy)))




(defun make-labels (plots)
  (send graph-proto :plots plots)
  (send graph-proto :add-mouse-mode 'draw-label
    :title "Draw Label"
    :cursor 'arrow
    :click :draw-label)

  (send graph-proto :add-mouse-mode 'erase-label
    :title "Erase Label"
    :cursor 'brush
    :click :erase-label)

  (send graph-proto :add-mouse-mode 'align-labels
    :title "Align Labels"
    :cursor 'finger
    :click :align-labels)

  (send graph-proto :add-mouse-mode 'get-drag-labels
    :title "Get Drag Labels"
    :cursor 'hand
    :click :get-drag-labels)
 
  (send graph-proto :add-mouse-mode 'drag-labels
    :title "Drag Labels"
    :cursor 'arrow
    :click :drag-labels)


 (let  (
        (v-h-ask (send text-item-proto :new "Place Label: "))
        (v-h-get (send choice-item-proto :new (list "Horizontally" "Vertically")
               :action #'(lambda () (send graph-proto :c-horiz-vert
                                        (send self :value)))))
        (h-just-ask (send text-item-proto :new "Horiz/Vert Justification:"))
        (h-just-get (send choice-item-proto :new 
                      (list "Left" "Centered" "Right")
                     :action #'(lambda () (send graph-proto :c-horiz 
                                            (send self :value)))))
        (v-just-ask (send text-item-proto :new "Vert/Horiz Justification"))
        (v-just-get (send choice-item-proto :new (list "Above" "Below")
                     :action #'(lambda () (send graph-proto :c-vert 
                                            (send self :value)))))
        (p-r-ask (send text-item-proto :new "Action:"))
        (p-r-get (send choice-item-proto :new 
               (list "Draw Labels" "Erase Labels" "Align Labels" "Drag Labels")
                  :action #'(lambda ()
                    (let ((p-r (send self :value))
                          (plots (send graph-proto :plots)))
                      (case p-r
                             (0 (mapcar #'(lambda (x) 
                                 (send x :mouse-mode 'draw-label)) plots))
                             (1 (mapcar #'(lambda (x) 
                                 (send x :mouse-mode 'erase-label)) plots))
                             (2 (mapcar #'(lambda (x)
                                 (send x :mouse-mode 'align-labels)) plots))
                             (3 (mapcar #'(lambda (x) 
                                 (send x :mouse-mode 'get-drag-labels)) 
                                   plots)))))))
       )
    (mapcar #'(lambda (x) (send x :mouse-mode 'draw-label)) plots)
    (send dialog-proto :new (list (list v-h-ask v-h-get)
                                  (list h-just-ask h-just-get)
                                  (list v-just-ask v-just-get)
                                  (list p-r-ask p-r-get)))))




(defmeth graph-proto :label-coordinates (&optional (val nil set))
 (if set (setf (slot-value 'label-coordinates) val)
    (slot-value 'label-coordinates)))

(defmeth graph-proto :plots-with-labels (&optional (val nil set))
 (if set (setf (slot-value 'plots-with-labels) val)
    (slot-value 'plots-with-labels)))

(defmeth graph-proto :plot-labels (&optional (val nil set))
 (if set (setf (slot-value 'plot-labels) val)
    (slot-value 'plot-labels)))

(defmeth graph-proto :horiz-vert (&optional (val nil set))
 (if set (setf (slot-value 'horiz-vert) val)
    (slot-value 'horiz-vert)))

(defmeth graph-proto :horiz (&optional (val nil set))
 (if set (setf (slot-value 'horiz) val)
    (slot-value 'horiz)))

(defmeth graph-proto :vert (&optional (val nil set))
 (if set (setf (slot-value 'vert) val)
    (slot-value 'vert)))

(defmeth graph-proto :c-horiz-vert (&optional (val nil set))
 (if set (setf (slot-value 'c-horiz-vert) val)
    (slot-value 'c-horiz-vert)))

(defmeth graph-proto :c-horiz (&optional (val nil set))
 (if set (setf (slot-value 'c-horiz) val)
    (slot-value 'c-horiz)))

(defmeth graph-proto :c-vert (&optional (val nil set))
 (if set (setf (slot-value 'c-vert) val)
    (slot-value 'c-vert)))
 
(defmeth graph-proto :plots (&optional (val nil set))
 (if set (setf (slot-value 'plots) val)
    (slot-value 'plots)))
 
(defmeth graph-proto :horiz-align (&optional (val nil set))
 (if set (setf (slot-value 'horiz-align) val)
    (slot-value 'horiz-align)))

(defmeth graph-proto :box-coords (&optional (val nil set))
 (if set (setf (slot-value 'box-coords) val)
    (slot-value 'box-coords)))

#|
(def x (normal-rand 10))
(def y (normal-rand 10))
(def z (normal-rand 10))
(def w (normal-rand 10))

(def p1 (plot-points x y))
(def p2 (plot-points z w))
;(def p3 (plot-points y w))
;(def p4 (histogram x))
;(def p5 (spin-plot (list x y z)))

(make-labels (list p1 p2))
|#


