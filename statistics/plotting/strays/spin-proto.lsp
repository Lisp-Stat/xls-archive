;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                        ;;
;;;;   Code that creates a spin plot with overlays that allows the user     ;;
;;;;   to insert, remove the regression plane; insert, remove projections;  ;;
;;;;   insert, remove hilighted points which also recomputes the regression ;;
;;;;   plane and projections.  Plots of residuals vs predicted and          ;;
;;;;   studentized-residuals vs. predicted are also produced.  Points can   ;;
;;;;   be colored by an index variable.  Residuals and Studentized          ;;
;;;;   residuals are also shown.  Plots are linked so case deletions can    ;;
;;;;   be made from any graph.                                              ;;
;;;;                                                                        ;;
;;;;   Comments, questions to Jason Bond (jbond@laplace.stat.ucla.edu)      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto better-spin-proto '(project frame plane plane-there adjust) 
                             () spin-proto)

(defproto add-frame-overlay nil nil graph-overlay-proto)
(defproto project-overlay nil nil graph-overlay-proto)
(defproto plane-overlay nil nil graph-overlay-proto)
(defproto return-overlay nil nil graph-overlay-proto)
(defproto delete-overlay nil nil graph-overlay-proto)


(defmeth better-spin-proto :start-plot ()
(let* (
       (n (send self :num-points))
       (x (send self :point-coordinate 0 (iseq n)))
       (y (send self :point-coordinate 1 (iseq n)))
       (z (send self :point-coordinate 2 (iseq n)))
       (plane (send self :plane))
      )
 (send self :linked t)
 (send self :margin 70 0 0 30)
 (send self :frame nil)
 (send self :plane-there nil)
 (cond ((not plane) 
         (def reg1 (regression-model (list x y) z))
         (send self :plane (send reg1 :coef-estimates))))
 (send self :transformation (matrix (list 3 3) (list 0 1 0 0 0 1 1 0 0)))
 (send self :add-overlay (send add-frame-overlay :new))
 (send self :add-overlay (send project-overlay :new))
 (send self :add-overlay (send plane-overlay :new))
 (send self :add-overlay (send return-overlay :new))
 (send self :add-overlay (send delete-overlay :new))
 )
)

(defmeth add-frame-overlay :redraw ()
 (let (
       (graph (send self :graph))
      )
   (send graph :frame-rect 10 10 10 10)
   (send graph :draw-string "Frame" 25 20)
 )
)

(defmeth add-frame-overlay :do-click (x y m1 m2)
(let* (
       (graph (send self :graph))
      )
  (cond ((and (< 10 x 20) (< 10 y 20))
          (send graph :frame (not (send graph :frame)))
          (cond  ((send graph :frame) (send graph :draw-frame))
                  (t (send graph :clear-lines)
                     (if (send graph :project) (send graph :project-points))
                     (if (send graph :plane-there) (send graph :draw-plane))
                     (send graph :redraw)))))

 )
)



(defmeth project-overlay :redraw ()
 (let ( 
       (graph (send self :graph))
      )
   (send graph :frame-rect 10 30 10 10)
   (send graph :draw-string "Proj" 25 40)
 )
)


(defmeth project-overlay :do-click (x y m1 m2)
 (let (
       (graph (send self :graph))
      )
    (cond ((and (< 10 x 20) (< 30 y 40))
            (send graph :project (not (send graph :project)))
            (send graph :clear-lines)
            (if (send graph :project) (send graph :project-points))
            (if (send graph :frame) (send graph :draw-frame))
            (if (send graph :plane-there) (send graph :draw-plane))))
 )
)

(defmeth plane-overlay :redraw ()
 (let (
       (graph (send self :graph))
      )
   (send graph :frame-rect 10 50 10 10)
   (send graph :draw-string "Plane" 25 60)
 )
)


(defmeth plane-overlay :do-click (x y m1 m2)
  (let (
        (graph (send self :graph))
       )
    (cond ((and (< 10 x 20) (< 50 y 60))
              (cond ((not (send graph :plane-there))
                      (send graph :draw-plane))
                    (t (send graph :clear-lines)
                       (if (send graph :frame) (send graph :draw-frame))
                       (if (send graph :project) (send graph :project-points))))
              (send graph :plane-there (not (send graph :plane-there)))))
  )
)

(defmeth delete-overlay :redraw ()
 (let ( 
       (graph (send self :graph))
      )
   (send graph :frame-rect 10 70 10 10)
   (send graph :draw-string "+/- Pts" 25 80)
 )
)


(defmeth delete-overlay :do-click (x y m1 m2)
 (let* (
        (graph (send self :graph))
        (n (send graph :num-points))
       )
    (cond ((and (< 10 x 20) (< 70 y 80))
           (cond ((= (length (send graph :points-selected)) 0)

                  (send graph :point-state (iseq n) 'normal)
                  (send graph :adjust (send graph :points-selected))

                  (send graph :clear-lines)
                                 
                  (if (send graph :frame) (send graph :draw-frame))
                  (if (send graph :plane-there) (send graph :draw-plane))
                  (if (send graph :project) (send graph :project-points))
                  (send graph :redraw))

                  (t (send graph :adjust (send graph :points-selected))
                     (send graph :point-state (send graph :points-selected)
                                                'invisible)
                     (send graph :clear-lines)
                     (if (send graph :frame) (send graph :draw-frame))
                     (if (send graph :plane-there) (send graph :draw-plane))
                     (if (send graph :project) (send graph :project-points))
                     (send graph :redraw)))))
 )
)






(defmeth return-overlay :redraw ()
 (let (
       (graph (send self :graph))
      )
   (send graph :frame-rect 10 90 10 10)
   (send graph :draw-string "Back" 25 100)
 )
)

(defmeth return-overlay :do-click (x y m1 m2)
  (let (
        (graph (send self :graph))
       )
    (if (and (< 10 x 20) (< 90 y 100))
            (send graph :transformation
               (matrix (list 3 3) (list 0 1 0 0 0 1 1 0 0))))
     (send self :redraw)
  )
)


(defmeth better-spin-proto :adjust-screen-point (i)
 (let* (
        (last-adjust (first (last (send self :adjust))))
       )
  (if last-adjust 
    (cond ((= i last-adjust) 
            (cond ((send self :needs-adjusting)
                     (send reg1 :included (send self :point-showing
                           (iseq (send self :num-points))))
                     (send residplot :clear-points)
                     (send studplot :clear-points)
                     (send residplot :add-points 
                       (list (send reg1 :fit-values) (send reg1 :residuals)))
                     (send studplot :add-points
                       (list (send reg1 :fit-values)
                             (send reg1 :studentized-residuals)))
                     (send studplot :adjust-to-data)
                     (send residplot :adjust-to-data)
                     (send residplot :points-showing 
                        (send self :points-showing))
                     (send studplot :points-showing 
                        (send self :points-showing))

                     (send reg1 :display)
                     (send self :plane (send reg1 :coef-estimates)))))))
  (call-next-method i)
 )
)



(defmeth better-spin-proto :draw-plane ()
  (let (
       (plane (send self :plane))
      )
  (setf plane (send self :plane))

  (send self :abcplane (first plane) (second plane) (third plane))
    (send self :redraw-background)
  (send self :redraw-overlays)
  (send self :redraw-content)
 )
)

(defmeth better-spin-proto :project-points ()
 (let* (
        (n (send self :num-points))
        (vars (send self :content-variables))
        (plane (send self :plane))
        (x (select (send self :point-coordinate (first vars) (iseq n))
              (send self :points-showing)))
        (y (select (send self :point-coordinate (second vars) (iseq n))
              (send self :points-showing)))
        (z (select (send self :point-coordinate (third vars) (iseq n))
              (send self :points-showing)))
        (newz (+ (first plane) (* (second plane) x)
                 (* (third plane) y)))
       )
     (mapcar #'(lambda (x y z1 z2) (send self :add-lines
                    (list (list x x) (list y y) (list z1 z2)))) x y z newz)
  )
)


(defmeth better-spin-proto :color-variable (color-variable)
  (let* (
         (vars (send self :content-variables))
         (n (send self :num-points))
         (color-var (send self :point-coordinate color-variable (iseq n)))
         (num-colors (length (remove-duplicates color-var)))
         (categories (remove-duplicates color-var))
         (num-colors (length categories))
         (colors (select (color-symbols) (list 6 5 3 7 1 2 0 4)))
         (colors (mapcar #'(lambda (x) (elt colors (position x categories))) 
                    color-var))
        )
    (send self :use-color t)
    (send self :point-color (iseq n) colors)
  )
)


(defmeth better-spin-proto :frame (&optional (val nil set))
 (if set (setf (slot-value 'frame) val)
  (slot-value 'frame))
)

(defmeth better-spin-proto :project (&optional (val nil set))
 (if set (setf (slot-value 'project) val)
  (slot-value 'project))
)

(defmeth better-spin-proto :plane (&optional (val nil set))
 (if set (setf (slot-value 'plane) val)
  (slot-value 'plane))
)

(defmeth better-spin-proto :plane-there (&optional (val nil set))
 (if set (setf (slot-value 'plane-there) val)
  (slot-value 'plane-there))
)

(defmeth better-spin-proto :adjust (&optional (val nil set))
 (if set (setf (slot-value 'adjust) val)
  (slot-value 'adjust))
)



(defmeth better-spin-proto :draw-frame ()
 (let* (
        (n (send self :num-points))
        (vars (send self :content-variables))
        (x (send self :point-coordinate (first vars) (iseq n)))
        (y (send self :point-coordinate (second vars) (iseq n)))
        (z (send self :point-coordinate (third vars) (iseq n)))
        (maxx (+ (max x) (* .1 (- (max x) (min x)))))
        (minx (- (min x) (* .1 (- (max x) (min x)))))
        (maxy (+ (max y) (* .1 (- (max y) (min y)))))
        (miny (- (min y) (* .1 (- (max y) (min y)))))
        (maxz (+ (max z) (* .1 (- (max z) (min z)))))
        (minz (- (min z) (* .1 (- (max z) (min z)))))
       )

  (mapcar #'(lambda (x) (send self :add-lines (list
              (list (elt x 0) (elt x 1)) (list (elt x 2) (elt x 3))
              (list (elt x 4) (elt x 5)))))
    (list (list minx minx miny maxy maxz maxz) 
          (list maxx maxx miny maxy maxz maxz)
          (list minx maxx miny miny maxz maxz)
          (list minx maxx maxy maxy maxz maxz)

          (list minx minx miny maxy minz minz) 
          (list maxx maxx miny maxy minz minz)
          (list minx maxx miny miny minz minz)
          (list minx maxx maxy maxy minz minz)
 
          (list minx minx miny miny minz maxz)
          (list maxx maxx miny miny minz maxz)
          (list minx minx maxy maxy minz maxz)
          (list maxx maxx maxy maxy minz maxz)))
  )
 (send self :redraw-background)
 (send self :redraw-overlays)
 (send self :redraw-content)
)


(defun spinit (vars &key (color-variable nil) (title "Graph") location 
                          (go-away t) menu (black-on-white nil)
                          has-h-scroll has-v-scroll menu-title
                          (variable-labels (list "X" "Y" "Z"))
                          (plane nil))

  (let (
        (sp (send better-spin-proto :new (length vars) 
              :variable-labels variable-labels :location location 
              :go-away go-away :menu menu :black-on-white black-on-white
              :has-h-scroll has-h-scroll :has-v-scroll has-v-scroll 
              :plane plane))
       )
    (send sp :add-points vars)
    (send sp :adjust-to-data)
    (if color-variable (send sp :color-variable color-variable))
    (send sp :start-plot)
    (subordinate-graphs (first vars) (second vars))

   sp
  )
)


(defun subordinate-graphs (x y)
    (def residplot (send scatterplot-proto :new 2 :variable-labels 
                    (list "Predicted" "Residuals")
              :title "Residuals vs. Predicted" :linked t))
    (def studplot (send scatterplot-proto :new 2 :variable-labels
                    (list "Predicted" "Studentized Residuals")
               :title "Studentized Residuals vs. Predicted" :linked t))
    
    (send residplot :add-points 
     (list (select (send reg1 :fit-values) 
                 (which (send reg1 :included (iseq (send reg1 :num-cases)))))
         (select (send reg1 :residuals)
                 (which (send reg1 :included (iseq (send reg1 :num-cases)))))))

    (send studplot :add-points 
     (list (select (send reg1 :fit-values)
                 (which (send reg1 :included (iseq (send reg1 :num-cases)))))
         (select (send reg1 :studentized-residuals)
                 (which (send reg1 :included (iseq (send reg1 :num-cases)))))))

    (send residplot :adjust-to-data)
    (send residplot :linked t)
    (send studplot :adjust-to-data)
    (send studplot :linked t)
  
)    


(def x1 (* 20 (normal-rand 30)))
(def x2 (* 30 (normal-rand 30)))
(def x3 (combine (repeat 1 10) (repeat 5 10) (repeat 14 10)))
(def x4 (normal-rand 30))
(def x5 (normal-rand 30))
(def spin (spinit (list x1 x2 x3 x4 x5) :color-variable 2))

