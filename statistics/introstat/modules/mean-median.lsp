
(defun meanmedian (&optional (data (exp (normal-rand 25))))
"Arg: optional DATA, a list of numbers.
A graph window will appear depicting the data points toghether 
with the mean and the median. 
You can move the points or add new points (see mouse mode in the menu)
and watch the central statistics changing."
(let* ((gr (send graph-proto :new 2
                 :title "Mean and median"
                 :show nil))
       (mitj (mean data))
       (fn (fivnum data)))
  (send gr :add-slot 'data)
  (send gr :add-slot 'mime)
  (send gr :add-slot 'prev-mime)
  (send gr :add-slot 'prev-cimu nil) 
  (send gr :add-slot 'prev-cimed nil)

  (defmeth gr :data (&optional (dt nil set))
    (when set
          (setf (slot-value 'data) dt))
    (when (setf dt (slot-value 'data))
          (let ((mitj (mean dt))
                (med (nth 2 (fivnum dt))))
            (setf (slot-value 'prev-mime) (slot-value 'mime))
            (setf (slot-value 'mime) (list mitj med))))
    (slot-value 'data))
  (defmeth gr :do-point-moving (x y a b)
    (let ((p (send self :drag-point x y :draw nil)))
      (when p 
            (let* ((pts (send self :point-coordinate
                              0 (iseq (send self :num-points)))))
              (send self :point-coordinate
                    1 (iseq (send self :num-points)) 0)
              (send self :data pts))
            (send self :redraw))))
  (defmeth gr :do-add-point (x y m1 m2)
    (let* (
           (xy (send self :canvas-to-real x y))
           (num (send self :num-points))
           (there-was-selection (send self :selection))
           )
      (send self :add-points (list (list (nth 0 xy)) '(0)))
      (send self :point-symbol num 'X)
      (if there-was-selection (send self :point-selected num t))
      (send self :data (send self :point-coordinate
                             0 (iseq (send self :num-points))))
      (send self :redraw)
      ))
  (defmeth gr :redraw ()
    (flet ((expand-ival (bounds &optional (factor 1.5))
                        (let* ((mid (mean bounds))
                               (wd (second (- bounds mid))))
                          (list (- mid (* factor wd))
                                (+ mid (* factor wd))))))
          (let* ((dt (send self :slot-value 'data))
                 (mitj (first (slot-value 'mime)))
                 (med (second (slot-value 'mime)))
                 (minmax (list (min dt) (max dt)))
                 (meanlabel (send self :real-to-canvas mitj 0.45))
                 (medlabel (send self :real-to-canvas med 0.30)))
            (send self :clear-lines)
            (send self :add-lines (list (list mitj mitj)
                                        (list -0.3 0.5)))
            (send self :add-lines (list (list med med)
                                        (list -0.1 0.3)))
            (send self :add-lines (list (expand-ival minmax) '(0 0)))
            (mapcar #'(lambda (pt) (send self :add-lines
                                         (list (list pt pt)
                                               '(-0.05 0.05))))
                    dt)
            (apply #'send self :draw-string "mean" meanlabel)
            (apply #'send self :draw-string "median" medlabel)
            (when (slot-value 'prev-mime)
                  (unless (= mitj (first (slot-value 'prev-mime)))
                          (setf mitj (first (slot-value 'prev-mime)))
                          (send self :add-lines (list (list mitj mitj)
                                                      (list -0.3 0.5))
                                :type 'dashed))
                  (unless (= med (second (slot-value 'prev-mime)))
                          (setf med (second (slot-value 'prev-mime)))
                          (send self :add-lines (list (list med med)
                                                      (list -0.1 0.3))
                                :type 'dashed)))
            (send self :draw-ci))))
(defmeth gr :boot-med ()
  (let* (
         (data (send self :data))
         (n (length data))  
         (bootlist (list ))
        )
    (dotimes (i 300)
     (let* (
            (samplei (sample data n t))
            (mediani (median samplei))
           )
        (setf bootlist (append bootlist (list mediani)))
      )
    )
   (list (quantile bootlist .05) (quantile bootlist .95))
  )
)

(defmeth gr :draw-ci ()
  (let* (
         (data (send self :data))
         (n (length data))
         (xbar (mean data))
         (sd (standard-deviation data))
         (zstar (normal-quant .975))
         (cimu (list (- xbar (/ (* zstar sd) (^ n .5)))
                   (+ xbar (/ (* zstar sd) (^ n .5)))))
         (cimed (send self :boot-med))
        )
    (send self :add-lines (list (list (first cimed) (second cimed))
                                (list .2 .2)))
    (send self :add-lines (list (list (first cimed) (first cimed))
                                (list .19 .21)))
    (send self :add-lines (list (list (second cimed) (second cimed))
                                (list .19 .21)))

    (send self :add-lines (list (list (first cimu) (second cimu)) 
                                (list (- .2) (- .2))))
    (send self :add-lines (list (list (first cimu) (first cimu)) 
                                (list (- .19) (- .21))))
    (send self :add-lines (list (list (second cimu) (second cimu))
                                (list (- .19) (- .21))))
    (apply #'send self :draw-string "95% CI for Mu"
                       (send self :real-to-canvas (first cimu) -.25))

    (apply #'send self :draw-string "95% BS CI for Median"
                       (send self :real-to-canvas (first cimed) .21))

    (cond ((and (slot-value 'prev-cimu) (slot-value 'prev-cimed))
           (let (
                 (oldcimu (slot-value 'prev-cimu))
                 (oldcimed (slot-value 'prev-cimed))
                )

          (send self :add-lines (list (list (first oldcimed) (second oldcimed))
                                      (list .1 .1) :type 'dashed))
          (send self :add-lines (list (list (first oldcimed) (first oldcimed))
                                      (list .09 .11) :type 'dashed))
          (send self :add-lines (list (list (second oldcimed) (second oldcimed))
                                      (list .09 .11) :type 'dashed))

          (send self :add-lines (list (list (first oldcimu) (second oldcimu)) 
                                            (list -.1 -.1)) :type 'dashed)
          (send self :add-lines (list (list (first oldcimu) (first oldcimu)) 
                                            (list -.09 -.11)) :type 'dashed)
          (send self :add-lines (list (list (second oldcimu) (second oldcimu))
                                            (list -.09 -.11)) :type 'dashed)
           ))
   )
    (setf (slot-value 'prev-cimu) cimu)
    (setf (slot-value 'prev-cimed) cimed)
  )
 )
        
        (send gr :data data)
        (send gr :add-points (list data (repeat 0 (length data))))
        (send gr :add-mouse-mode 'point-moving
              :title "Point moving"
              :cursor 'finger
              :click :do-point-moving)
        (unless (member 'add-point (send gr :mouse-modes))
                (send gr :add-mouse-mode 'add-points
                      :title "Add new points"
                      :cursor 'finger
                      :click :do-add-point))
        (send gr :adjust-to-data)
        (send gr :show-window)
        (send gr :adjust-to-data)
        (send gr :mouse-mode 'point-moving)
        gr))


