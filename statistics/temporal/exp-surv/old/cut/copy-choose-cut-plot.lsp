; Plot for choosing cut point in covariate.
;
(provide 'copy-choose-cut-plot)
(load "kmest")
(load "km-plot")
(load "density")
(load "line-density2")
;
;MEG 8/91
;This function graphs the initial data and sets up the functions
;  to run the slider to cut the data.  It also calls the density 
;  estimate function and linewidth function if specified
;
(defun COPY-CHOOSE-CUT-PLOT (time stat covar)
  (setf plot (send graph-proto :new 2))
  (send plot :start-buffering)
  (let* ((data (km-plot time stat (kmest time stat)))
         (choose-slider (sequence-slider-dialog 
                         (rseq (min covar) (max covar) 50)
                         :action
                         #'(lambda (cut) (send plot :set-cut cut)))))

    (send plot :add-lines (list (first data) (second data)))
    (if (eql lwid t) (line-density2 (first data) 0 (send plot :num-lines)))
    (send plot :variable-label '(0 1) (list "Time" "Proportion Surviving"))
    (send plot :x-axis t t 5) (send plot :y-axis t t 5)
    (send plot :title "Kaplan-Meier Plot")
    (send plot :use-color t)
   
    (defmeth plot :set-cut (cut)
      (prog* ((upper (> covar cut))
              (which-upper (which upper))
              (which-lower (which (mapcar #'not upper)))
              (time-upper (select time which-upper))
              (stat-upper (select stat which-upper))
              (time-lower (select time which-lower))
              (stat-lower (select stat which-lower))
              (data-upper (km-plot time-upper stat-upper
                                   (kmest time-upper stat-upper)))
              (data-lower (km-plot time-lower stat-lower
                                   (kmest time-lower stat-lower))))
             
             (def pee (length (first data-lower)))
             (def in1 (iseq 0 (- pee 1)))
             (def nee (length (first data-upper)))
             (def in3 (iseq pee (- (+ nee pee) 2)))
             (def in2 (iseq  (- (+ nee pee) 2) pee))
           
             (send self :linestart-coordinate 0 in1 (select (first data-lower) in1))
             (send self :linestart-coordinate 1 in1 (select (second data-lower) in1))
             (send self :linestart-coordinate 0 in3  (select (first data-upper) (- in2 pee)))
             (send self :linestart-coordinate 1 in3 (select (second data-upper) (- in2 pee)))
             
             (make-color 'blue1 0 0.2 0.7)
             (make-color 'purple1 0.5 0 0.4)
             (send self :linestart-color (iseq 0 (- pee 1)) 'blue1)
             (send self :linestart-color (iseq pee (- (+ pee nee) 2)) 'purple1)
             (send self :redraw-content)
             (if (eql de t) (send den :adjust-to-density cut))))

    (if (eql de t) (density))
   
    (send choose-slider :value 25)
    (send choose-slider :location 285 220)
    (send choose-slider :title "Covariate Cut")
    (send plot :adjust-to-data)
    (send plot :set-cut (elt (rseq (min covar) (max covar) 50) 25)) 
    (defmeth plot :close ()
      (undef (variables))
      (call-next-method))
    (send plot :buffer-to-screen) plot))


