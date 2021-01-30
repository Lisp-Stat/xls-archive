;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Zoom Overlay prototype.  When the "+" slider button is pushed
;;  in the overlay, a box can be drawn around points (using the mouse
;;  mouse) and they will be "zoomed" in on.  To incrementally zoom out
;;  push the "-" button.  The "Out" button completely zooms out.  Selecting
;;  is handled in the it way it should be.  The shift key keeps points
;;  selected while selecting others and this is true at any zooming stage.
;;  Points that are on top of each other can be expanded to show their
;;  labels.  The up and down arrows control the amount of expansion.
;;  Any other clicks in either the plot or the overlays revert points
;;  to their original positions.  Other niceties inlcude adding/removing
;;  zoom-overlays in the plots menu and the option to include a list of 1's
;;  and 0's indicating selectedness/de-selectedness of points in the plots
;;  menu.  The overlay is added by sending the original plot the :add-overlay
;;  method.  An example is shown at the bottom.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Zoom Overlay Prototype
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto zoom-overlay-proto nil nil graph-overlay-proto)

(defmeth zoom-overlay-proto :setup ()
  (let* ((graph (send self :graph))
         (margin (send graph :margin)))
    (send graph :add-mouse-mode 'zoom
                :title "Zooming" :cursor 'hand :click :get-zoom-points)
    (send graph :add-mouse-mode 'newselecting
                :title "New Selecting" :cursor 'arrow :click :newselect)
    (send graph :add-mouse-mode 'sep
                :title "Point Expand" :cursor 'finger :click :move-seperate)
    (send graph :add-slot 'seperate)
    (send graph :add-slot 'seperate-pt-coords)
    (send graph :add-slot 'seperate-pt-inds)
    (send graph :add-slot 'overlay-list)
    (send graph :add-slot 'bitmap)
    (send graph :add-slot 'selecting)
    (send graph :add-slot 'zoom)
    (send graph :add-slot 'pts-showing)
    (send graph :add-slot 'pts-selected)
    (send graph :add-slot 'increment-position)
    (send graph :add-slot 'increment-intervals)
    (normal-accessor :zoom zoom graph)
    (normal-accessor :seperate seperate graph)
    (normal-accessor :seperate-pt-coords seperate-pt-coords graph)
    (normal-accessor :seperate-pt-inds seperate-pt-inds graph)
    (normal-accessor :selecting selecting graph)
    (normal-accessor :overlay-list overlay-list graph)
    (normal-accessor :increment-position increment-position graph)
    (normal-accessor :increment-intervals increment-intervals graph)
    (send graph :overlay-list (list self))
    (send graph :pts-showing :pts (iseq (send graph :num-points)))
    (send graph :mouse-mode 'newselecting)
    (send graph :make-menu)
    (when (< (first margin) (* 2.5 (send graph :text-width "Zoom")))
          (setf (elt margin 0)
                (floor (* 2.5 (send graph :text-width "Zoom"))))
          (apply #'send graph :margin margin))
))



(defmeth zoom-overlay-proto :redraw ()
 (let* (
       (graph (send self :graph))
       (ht (send graph :text-ascent))
      )
   (unless (send graph :has-slot 'zoom) (send self :setup))
   (send self :bitmap (send graph :zoom) nil nil (send graph :seperate))
   (send graph :frame-rect 9 (1- ht) 12 (+ 40 ht))
   (send graph :draw-string "Out" 25 (+ 10 ht))
   (send graph :draw-string "+" 25 (+ 30 ht))
   (send graph :draw-string "-" 25 (+ 50 ht))
   (send graph :draw-string "Select" 25 (+ 70 ht))
   (send graph :draw-string "Expand" 25 (+ 110 ht))))



(defmeth zoom-overlay-proto :do-click (x y m1 m2)
 (let* ((graph (send self :graph))
        (zoom (send graph :zoom))
        (ht (send graph :text-ascent)))
  (cond ((and (< 10 x 20) (< ht y (+ 10 ht)) zoom)
         (let ((pts-selected (first (last 
                   (send graph :pts-selected)))))
          (send graph :zoom nil)
          (if (send graph :seperate) (send self :revert))
          (send graph :mouse-mode 'newselecting)
          (send graph :show-all-points)
          (send graph :slot-value 'pts-showing nil)
          (send graph :pts-showing :pts (iseq (send graph :num-points)))
          (send graph :clear-masks)
          (if pts-selected
              (send graph :point-selected pts-selected
                                      (repeat 't (length pts-selected))))
          (send graph :selecting nil)
          (send graph :adjust-to-data)
          (send self :bitmap nil)))
        ((and (< 10 x 20) (< (+ 20 ht) y (+ 30 ht)) (not zoom))
          (send graph :zoom 't)
          (if (send graph :seperate) (send self :revert))
          (send self :bitmap 't)
          (send graph :mouse-mode 'zoom))
        ((and (< 10 x 20) (< (+ 40 ht) y (+ 50 ht)))
          (send graph :zoom 't)
          (if (send graph :seperate) (send self :revert))
          (send self :bitmap 't 't)
          (pause 5)
          (if (> (length (send graph :pts-showing)) 1)
              (send graph :step-out))
          (send graph :selecting nil)
          (send graph :mouse-mode 'zoom)
          (send graph :adjust-to-data)
          (send self :bitmap 't))
        ((and (< 10 x 20) (< (+ 60 ht) y (+ 70 ht)))
         (if (send graph :seperate) 
             (let ((pts-selected (intersection
                      (first (last (send graph :pts-selected)))
                      (first (last (send graph :pts-showing))))))
                (send self :revert)
                (if pts-selected
                   (send graph :point-selected pts-selected
                                      (repeat 't (length pts-selected))))))
         (send graph :selecting (not (send graph :selecting)))
         (if (send graph :selecting) 
             (send graph :mouse-mode 'newselecting)
             (progn 
              (if (send graph :zoom)
                  (send graph :mouse-mode 'zoom)
                  (send graph :mouse-mode 'newselecting))))
         (send self :bitmap (send graph :zoom) nil (send graph :selecting)))
        ((and (< 10 x 20) (< (+ 100 ht) y (+ 110 ht)))
         (if (send graph :seperate)
             (let ((pts-selected (intersection
                      (first (last (send graph :pts-selected)))
                      (first (last (send graph :pts-showing))))))
                (send self :revert)
                (if pts-selected
                   (send graph :point-selected pts-selected
                                      (repeat 't (length pts-selected)))))
             (progn
               (send graph :selecting nil)
               (send graph :seperate 't)
               (send self :bitmap (send graph :zoom) nil nil t)
               (send graph :mouse-mode 'sep))))
        ((and (< 8 x 22) (< (+ ht 83) y (+ 98 ht)))
         (when (and (send graph :seperate)
                  (send graph :increment-position))

             (send self :bitmap (send graph :zoom) nil nil 't 't nil)
             (send graph :increment-seperate 't)
             (send self :bitmap (send graph :zoom) nil nil 't nil nil)))
        ((and (< 8 x 22) (< (+ ht 112) y (+ 127 ht)))
         (when (and (send graph :seperate)
                  (send graph :increment-position))
             (send self :bitmap (send graph :zoom) nil nil 't nil 't)
             (send graph :increment-seperate nil)
             (send self :bitmap (send graph :zoom) nil nil 't nil nil))))))




(defmeth zoom-overlay-proto :revert ()
 (let* ((graph (send self :graph))
        (zoom (send graph :zoom))
        (pts (send graph :seperate-pt-coords)))
   (send graph :seperate nil)
   (if zoom
       (progn
          (send graph :mouse-mode 'zoom)
          (send self :bitmap 't))
       (progn
          (send graph :mouse-mode 'newselecting)
          (send graph :show-all-points)
          (send graph :adjust-to-data)
          (send self :bitmap nil)))
   (if pts (send graph :revert-pts))))


  
(defmeth zoom-overlay-proto :bitmap (zoom &optional out sel sep up down)
 (let* ((check-bitmap (bullet))
        (blank-bitmap (empty))
        (check (check))
        (nocheck (nocheck))
        (graph (send self :graph))
        (up-sel-arrow (up-sel-arrow))
        (up-arrow (up-arrow))
        (down-sel-arrow (down-sel-arrow))
        (down-arrow (down-arrow))
        (ht (send graph :text-ascent)))
    (send graph :draw-bitmap (if zoom 
                                 blank-bitmap check-bitmap) 10 ht)
    (send graph :draw-bitmap (if (and zoom (not out))
                    check-bitmap blank-bitmap) 10 (+ 20 ht))
    (send graph :draw-bitmap (if out
                    check-bitmap blank-bitmap) 10 (+ 40 ht))
    (send graph :draw-bitmap (if sel check nocheck) 10 (+ 60 ht))
    (send graph :draw-bitmap (if sep check nocheck) 10 (+ 100 ht))
    (send graph :draw-bitmap (if up up-sel-arrow up-arrow) 8 (+ 83 ht))
    (send graph :draw-bitmap (if down down-sel-arrow down-arrow) 
                   8 (+ 113 ht))))



           

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Scatterplot Methods
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth graph-proto :revert-pts ()
 (let ((nv (send self :num-variables))
       (pts (send self :seperate-pt-coords))
       (inds (send self :seperate-pt-inds)))
   (when (and inds pts)
         (mapcar #'(lambda (x)
           (send self :point-coordinate x inds (elt pts x))) (iseq nv))
         (mapcar #'(lambda (x) (send self :scale x 1)) (iseq nv))
         (send self :adjust-to-data)
         (send self :seperate-pt-coords nil)
         (send self :seperate-pt-inds nil))))
       

(defmeth graph-proto :move-seperate (x y m1 m2)
(when (and (< (elt (send self :margin) 0) x) 
           (< (elt (send self :margin) 1) y))
(send self :increment-position nil)
(send self :revert-pts)
(let* ((coords (send self :draw-box x y))
       (pts (send self :points-in-rect
                       (if (< x (first coords)) x (first coords))
                       (if (< y (second coords)) y (second coords))
                       (abs (- x (first coords)))
                       (abs (- y (second coords))))))
 (if pts 
  (let* ((nv (send self :num-variables))
         (ranges (send self :range (iseq nv)))
         (coord-dists (mapcar #'(lambda (x) (- (second x) (first x))) ranges))
         (mns (mapcar #'(lambda (x) (mean (send self :point-coordinate x pts)))
                 (iseq nv)))
         (nranges (mapcar #'(lambda (x y) (list (- x (/ y 2)) (+ x (/ y 2))))
                       mns coord-dists))
         (coords (mapcar #'(lambda (x) (send self :point-coordinate x pts))
                   (iseq nv)))
         (increment (/ 360 (length pts)))
         (angle (* (/ (* increment (iseq 1 (length pts))) 180) pi))
         (expand (/ coord-dists 20))
         (intervals (mapcar #'(lambda (x y)
                  (list (rseq (first x) (+ (first x) (/ y 1.5)) 20)
                        (rseq (second x) (- (second x) (/ y 1.5)) 20)))
                     nranges coord-dists)))
     (send self :seperate-pt-inds pts)
     (send self :seperate-pt-coords coords)
     (mapcar #'(lambda (x y) (send self :range x (first y) (second y)))
        (iseq nv) nranges)
     (mapcar #'(lambda (x y) (send self :point-coordinate x pts
                              (+ (elt coords x) 
                              (* y (if (= x 0) (cos angle) (sin angle))))))
              (iseq nv) expand)
     (send self :increment-position 0)
     (send self :increment-intervals intervals)
     (send self :redraw))))))
 

(defmeth graph-proto :increment-seperate (up)
 (let* ((inc-pos (send self :increment-position))
        (inc-intervals (send self :increment-intervals))
        (len (length (first (first inc-intervals)))))
   (when (or (and up (< inc-pos (- len 1)))
             (and (not up) (> inc-pos 0)))
   (mapcar #'(lambda (x y) (send self :range x
          (if up (elt (first y) (1+ inc-pos)) (elt (first y) (1- inc-pos)))
          (if up (elt (second y) (1+ inc-pos)) (elt (second y) (1- inc-pos)))))
     (iseq 3) inc-intervals)
   (send self :increment-position (if up (1+ inc-pos) (1- inc-pos))))))




(defmeth graph-proto :get-zoom-points (x y m1 m2)
 (let* (
        (coords (send self :draw-box x y))
        (num-lines (send self :num-lines))
        (pts (send self :points-in-rect
                        (if (< x (first coords)) x (first coords))
                        (if (< y (second coords)) y (second coords))
                        (abs (- x (first coords)))
                        (abs (- y (second coords)))))
        (pts-showing (send self :points-showing)))
    (if pts (send self :pts-showing :pts pts))
    (when (and pts (not (= (length pts) (length pts-showing))))
      (send self :point-showing
         (sort-data (set-difference pts-showing pts))
         (repeat nil (- (length pts-showing) (length pts))))
      (send self :adjust-to-data))))


(defmeth graph-proto :draw-box (x y)
 (let ((newx x)
       (newy y))
 (send self :draw-mode 'xor)
 (send self :while-button-down #'(lambda (x1 y1)
                      (send self :draw-box-lines x y x1 y1)
                      (send self :draw-box-lines x y newx newy)
                      (setf newx x1)
                      (setf newy y1)))
 (send self :draw-box-lines x y newx newy)
 (send self :draw-mode 'normal)
 (list newx newy)))

(defmeth graph-proto :draw-box-lines (x y x1 y1)
  (send self :draw-line x y x y1)
  (send self :draw-line x y x1 y)
  (send self :draw-line x y1 x1 y1)
  (send self :draw-line x1 y1 x1 y)
)




(defmeth graph-proto :newselect (x y m1 m2)
 (let ((margin (send self :margin)))
  (when (and (< (elt margin 0) x) (< (elt margin 1) y))
    (let* ((cl (send self :click-range))
           (coords (send self :draw-box x y))
           (pts (union
                 (send self :points-in-rect
                           (if (< x (first coords)) x (first coords))
                           (if (< y (second coords)) y (second coords))
                           (abs (- x (first coords)))
                           (abs (- y (second coords))))
                 (send self :points-in-rect
                           (floor (- x (/ (first cl) 2)))
                           (floor (- y (/ (second cl) 2)))
                           (first cl) (second cl)))))
      (unless m1 (send self :unselect-all-points))
      (send self :pts-selected :pts pts :mod m1)
      (let ((newpts (intersection
                    (first (last (send self :pts-selected)))
                    (first (last (send self :pts-showing))))))
       (if newpts (send self :point-selected newpts 't)))))))



(defmeth graph-proto :pts-selected (&key (pts nil set) step-out mod)
 (let ((pts-selected (slot-value 'pts-selected)))
  (cond (set
         (setf (slot-value 'pts-selected)
               (if mod
                (if pts
                    (list (union pts (first (last pts-selected))))
                    pts-selected)
                (list pts))))
          (step-out
             (if (> (length pts-selected) 1)
                 (let ((first-end (first (last pts-selected)))
                       (second-end (first (last (butlast pts-selected)))))
                   (setf (slot-value 'pts-selected)
                         (append (butlast (butlast pts-selected))
                                     (list (union first-end second-end)))))))
          (t pts-selected))))



(defmeth graph-proto :pts-showing (&key pts step-out)
 (let ((pts-showing (slot-value 'pts-showing)))
   (cond (pts
             (if pts-showing
              (progn
               (setf (first (last pts-showing))
                      (set-difference (first (last pts-showing)) pts))
               (setf (slot-value 'pts-showing) (append pts-showing (list pts))))
              (setf (slot-value 'pts-showing) (list pts))))
          (step-out
             (if (> (length pts-showing) 1)
                 (let ((first-end (first (last pts-showing)))
                       (second-end (first (last (butlast pts-showing)))))
                   (setf (slot-value 'pts-showing)
                         (append (butlast (butlast pts-showing))
                                     (list (union first-end second-end)))))))
          (t pts-showing))))



(defmeth graph-proto :step-out ()
  (send self :pts-showing :step-out 't)
  (send self :pts-selected :step-out 't)
  (let ((pts-showing (first (last (send self :pts-showing))))
        (pts-selected (first (last (send self :pts-selected)))))
    (send self :point-showing pts-showing (repeat 't (length pts-showing)))
    (if pts-selected
      (send self :point-selected (intersection pts-showing pts-selected) 't))))



(defmeth graph-proto :make-menu ()
  (flet ((remove-overlays ()
            (mapcar #'(lambda (x) (send self :delete-overlay x))
                  (send self :overlay-list))
              (if (kind-of-p self spin-proto)
                    (send self :margin 0 0 0 25)
                    (send self :margin 20 0 0 0)))
        (replace-overlays ()
            (let ((wid (send self :text-width "Zoom")))
              (if (kind-of-p self spin-proto)
                    (send self :margin (floor (* 2.5 wid)) 0 0 25)
                    (send self :margin (floor (* 2.5 wid)) 0 0 0))
              (mapcar #'(lambda (x) (send self :add-overlay x))
                 (send self :overlay-list))
              (send self :redraw-overlays)))
        (actiona () (send self :get-selection-list)))
    (let* (
           (itema (send menu-item-proto
                    :new "Selection List" :action #'actiona))
           (dash (send dash-item-proto :new))
           (itemb (send menu-item-proto
                 :new "Overlays" :action #'(lambda ()
                    (let ((val (slot-value 'mark)))
                     (if val (remove-overlays) (replace-overlays))
                     (setf (slot-value 'mark) (not val))))))
           (old-menu (send self :menu))
           (menu-items (send old-menu :items)))
       (send itemb :mark 't)
       (send old-menu :slot-value 'items (append menu-items
                     (list itemb dash itema))))))



(defmeth graph-proto :get-selection-list ()
 (let* ((list-name (get-string-dialog "Name of Variable: "))
        (list-value (symbol-value (intern (string-upcase list-name))))
        (num-points (send self :num-points)))
   (when (and (= (length list-value) num-points)
                 (every #'numberp list-value))
         (send self :unselect-all-points)
         (send self :pts-selected :pts (which (= 1 list-value)))
         (let ((newpts (first (last (send self :pts-selected)))))
               (if newpts (send self :point-selected newpts 't))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Macro for creating accessor methods and bitmaps
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro normal-accessor (key slot prototype)
`(defmeth ,prototype ,key (&optional (content nil set))
   (when set (setf (slot-value ',slot) content))
   (slot-value ',slot)))


(defun bullet ()
'#2a((0 0 0 0 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0)
     (0 0 1 1 1 1 1 1 0 0)
     (0 0 1 1 1 1 1 1 0 0)
     (0 0 1 1 1 1 1 1 0 0)
     (0 0 1 1 1 1 1 1 0 0)
     (0 0 1 1 1 1 1 1 0 0)
     (0 0 1 1 1 1 1 1 0 0)
     (0 0 0 0 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0)))

(defun empty ()
'#2a((0 0 0 0 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0)))

(defun check ()
'#2a((1 1 1 1 1 1 1 1 1 1)
     (1 1 0 0 0 0 0 0 1 1)
     (1 0 1 0 0 0 0 1 0 1)
     (1 0 0 1 0 0 1 0 0 1)
     (1 0 0 0 1 1 0 0 0 1)
     (1 0 0 0 1 1 0 0 0 1)
     (1 0 0 1 0 0 1 0 0 1)
     (1 0 1 0 0 0 0 1 0 1)
     (1 1 0 0 0 0 0 0 1 1)
     (1 1 1 1 1 1 1 1 1 1)))

(defun nocheck ()
'#2a((1 1 1 1 1 1 1 1 1 1)
     (1 0 0 0 0 0 0 0 0 1)
     (1 0 0 0 0 0 0 0 0 1)
     (1 0 0 0 0 0 0 0 0 1)
     (1 0 0 0 0 0 0 0 0 1)
     (1 0 0 0 0 0 0 0 0 1)
     (1 0 0 0 0 0 0 0 0 1)
     (1 0 0 0 0 0 0 0 0 1)
     (1 0 0 0 0 0 0 0 0 1)
     (1 1 1 1 1 1 1 1 1 1)))


(defun up-sel-arrow ()
'#2a((0 0 0 0 0 0 1 1 0 0 0 0 0 0)
     (0 0 0 0 0 1 1 1 1 0 0 0 0 0)
     (0 0 0 0 1 1 1 1 1 1 0 0 0 0)
     (0 0 0 1 1 1 1 1 1 1 1 0 0 0)
     (0 0 1 1 1 1 1 1 1 1 1 1 0 0)
     (0 1 0 1 1 1 1 1 1 1 1 0 1 0)
     (1 0 0 1 1 1 1 1 1 1 1 0 0 1)
     (0 0 0 1 1 1 1 1 1 1 1 0 0 0)
     (0 0 0 1 1 1 1 1 1 1 1 0 0 0)
     (0 0 0 1 1 1 1 1 1 1 1 0 0 0)
     (0 0 0 1 1 1 1 1 1 1 1 0 0 0)
     (0 0 0 1 1 1 1 1 1 1 1 0 0 0)
     (0 0 0 1 1 1 1 1 1 1 1 0 0 0)
     (0 0 0 1 1 1 1 1 1 1 1 0 0 0)))

(defun up-arrow ()
'#2a((0 0 0 0 0 0 1 1 0 0 0 0 0 0)
     (0 0 0 0 0 1 1 1 1 0 0 0 0 0)
     (0 0 0 0 1 1 1 1 1 1 0 0 0 0)
     (0 0 0 1 1 1 1 1 1 1 1 0 0 0)
     (0 0 1 1 0 0 0 0 0 0 1 1 0 0)
     (0 1 0 1 0 0 0 0 0 0 1 0 1 0)
     (1 0 0 1 0 0 0 0 0 0 1 0 0 1)
     (0 0 0 1 0 0 0 0 0 0 1 0 0 0)
     (0 0 0 1 0 0 0 0 0 0 1 0 0 0)
     (0 0 0 1 0 0 0 0 0 0 1 0 0 0)
     (0 0 0 1 0 0 0 0 0 0 1 0 0 0)
     (0 0 0 1 0 0 0 0 0 0 1 0 0 0)
     (0 0 0 1 0 0 0 0 0 0 1 0 0 0)
     (0 0 0 1 1 1 1 1 1 1 1 0 0 0)))



(defun down-sel-arrow ()
'#2a((0 0 0 1 1 1 1 1 1 1 1 0 0 0)
     (0 0 0 1 1 1 1 1 1 1 1 0 0 0)
     (0 0 0 1 1 1 1 1 1 1 1 0 0 0)
     (0 0 0 1 1 1 1 1 1 1 1 0 0 0)
     (0 0 0 1 1 1 1 1 1 1 1 0 0 0)
     (0 0 0 1 1 1 1 1 1 1 1 0 0 0)
     (0 0 0 1 1 1 1 1 1 1 1 0 0 0)
     (1 0 0 1 1 1 1 1 1 1 1 0 0 1)
     (0 1 0 1 1 1 1 1 1 1 1 0 1 0)
     (0 0 1 1 1 1 1 1 1 1 1 1 0 0)
     (0 0 0 1 1 1 1 1 1 1 1 0 0 0)
     (0 0 0 0 1 1 1 1 1 1 0 0 0 0)
     (0 0 0 0 0 1 1 1 1 0 0 0 0 0)
     (0 0 0 0 0 0 1 1 0 0 0 0 0 0)))

(defun down-arrow ()
'#2a((0 0 0 1 1 1 1 1 1 1 1 0 0 0)
     (0 0 0 1 0 0 0 0 0 0 1 0 0 0)
     (0 0 0 1 0 0 0 0 0 0 1 0 0 0)
     (0 0 0 1 0 0 0 0 0 0 1 0 0 0)
     (0 0 0 1 0 0 0 0 0 0 1 0 0 0)
     (0 0 0 1 0 0 0 0 0 0 1 0 0 0)
     (0 0 0 1 0 0 0 0 0 0 1 0 0 0)
     (1 0 0 1 0 0 0 0 0 0 1 0 0 1)
     (0 1 0 1 0 0 0 0 0 0 1 0 1 0)
     (0 0 1 1 0 0 0 0 0 0 1 1 0 0)
     (0 0 0 1 1 1 1 1 1 1 1 0 0 0)
     (0 0 0 0 1 1 1 1 1 1 0 0 0 0)
     (0 0 0 0 0 1 1 1 1 0 0 0 0 0)
     (0 0 0 0 0 0 1 1 0 0 0 0 0 0)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  A 2-dimensional example 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (def p1 (plot-points (list (* 20 (normal-rand 2)) (* 5 (normal-rand 2)))))
 (send p1 :add-points (repeat 5 30) (repeat 6 30))
 (send p1 :add-points (repeat -1 2) (repeat -3 2))
 (send p1 :add-points (repeat 0 10) (repeat 0 10))
 (send p1 :add-points (repeat 3 30) (repeat 2 30))

 (send p1 :add-overlay (send zoom-overlay-proto :new))

 (send p1 :adjust-to-data)
