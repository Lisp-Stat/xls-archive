;
; performs Kaplan-Meier estimation.
;
(defun kmest (time status)
  (let* ((n (length time))
         (s 1)
         (surv nil))
    (dotimes (i n)
             (setf s (* s (- 1 (/ (select status i) (- n i)))))
             (setf surv (cons s surv)))
    (reverse surv)))
;
; transforms results of kmest into x,y
; coords for plotting.
;
(defun km-plot (time status surv)
  (let* (
         (n (length time))
         (rep-pat (repeat 2 n))
         (time-plot (cons 0 (repeat time rep-pat)))
         (surv-plot (reverse 
                     (cons (select surv (- n 1)) 
                           (repeat (reverse (cons 1 
                                                  (select 
                                                   surv 
                                                   (iseq (- n 1))))) 
                                   rep-pat)))))
    (list time-plot surv-plot)))
;
; generates a scatterplot matrix linked to
; a Kaplan-Meier estimate.
;
(defun scat-km (time status &rest covars)
  (let* ((scat (scatterplot-matrix covars))
         (data (km-plot time status (kmest time status)))
         (km (plot-lines (first data) (second data) :title "KM Plot"))
         (current-points (iseq (length time))))
    (send km :range 1 0 1)
    (send scat :point-state (iseq (length time)) 'hilited)
    (defmeth scat :unselect-all-points ()
      (setf current-points nil)
      (send km :clear)
      (call-next-method))
    (defmeth scat :adjust-points-in-rect (x1 y1 w h s)
      (let* ((p-i-r (send self :points-in-rect x1 y1 w h)))
        (setf current-points
              (case (send self :mouse-mode)
                (brushing p-i-r) 
                (selecting (union p-i-r current-points))))
        (if current-points
            (prog* 
             ((points (sort-data current-points))
              (time-sel (select time points))
              (status-sel (select status points))
              (new-plot (km-plot time-sel status-sel 
                                (kmest time-sel status-sel))))
             (send km :clear :draw nil)
             (send km :add-lines 
                   (first new-plot) (second new-plot))))
        (call-next-method x1 y1 w h s)))
    scat))
;
; this funtion generates exponentiallly
; distributed random variates with the
; specified values of lambda.
;
; ENA - 3/5/91.
;
(defun exp-rand (lambda)
  (/ (- (log (uniform-rand (length lambda)))) lambda))
;
; this routine sorts a list x into
; ascending order and also cosorts
; an optinal number of additional
; lists into the same order. For
; example, this can be used to sort
; survival times and retain the association
; of the survival times with covariate
; values.
; 
; ENA - 3/5/91.
;
(defun cosort (x &rest others)
  (let ((xorder (order x)))
    (mapcar #'(lambda (z) (select z xorder))
            (cons x others))))
;
; Given a list of symbols and a list of values,
; this function assigns the values to the symbols.
;
; ENA - 6/23/93.
;
(defun assign-vars (data vars)
  (if vars (mapcar #'myset vars data))
  nil
  )
;
; Reads in a data file and assigns the values
; to the specified variables.
;
; ENA - 6/23/93.
;
(defmacro get-data (&rest vars)
  (let ((data (read-data-columns)))
    (assign-vars data vars)
    )
  )
;
; This function sets the variable whose name is the value of name
; to the value val. Thus
;
;       (defvar a 'b)
;       (myset a 10)
;
; sets b to 10 while leaving a unchanged.
;
; ENA - 6/23/93.
;
(defun myset (name val)
  (setf (symbol-value name) val)
  )
;
; Prints the items in the list "line" to file
; "file" and then print a carriage return.
;
; ENA - 6/23/93.
;
(defun print-line (file line)
  (if line
      (progn
       (format file "~a " (first line))
       (print-line file (rest line))
       )
      (format file "~%")
      )
  )
;
; Prints the elements of list "list"
; to file "file" one element per line.
; The elements can themselves be lists.
;
; ENA - 06/23/93.
;
(defun print-list (file list)
  (if list
      (mapcar #'(lambda (item) (print-line file item)) list)
      )
  )
;
; Saves the specified variables to a file
; in a readable ascii format.
;
; ENA - 06/23/93.
;
#+macintosh (defun save-data (&rest vars)
  (let (
        (datafile 
         (open (set-file-dialog "Save data as:" "new file" t)
               :direction :output)))
    (print-list datafile (transpose vars))
    (close datafile)
    )
  )
#-macinstosh (defun save-data (&rest vars)
  (let (
        (datafile
         (open (get-string-dialog "Save data as:" :initial "new file")
               :direction :output)))
    (print-list datafile (transpose vars))
    (close datafile)
    )
  )
;
; Generate some data to play with.
; 
; This is for scat-event as well as scat-box and scat-km, 
; so we need arrival times and length of observation.
;
; x and y are uniform (0,1). z=x*y.
; times are distributed exponentially
; with lambda = 10x + y. 
;
; ENA - 6/2/92.
;
; modified 6/24/93 to use 0/1 as status rather than nil/T - ENA.
;
(defun gen-data (num)
  (let* 
    (
     (xt (uniform-rand num))
     (yt (uniform-rand num))
     (zt (* xt yt))
     (survt (exp-rand (+ (* 10 xt) yt)))
     (scale-survt (/ survt (standard-deviation survt)))
     (arrivet (* 2 (uniform-rand num)))
     (deatht (+ arrivet scale-survt))
     (lastt (map 'list #'(lambda (x) (min 2 x)) deatht))
     (lengtht (- lastt arrivet))
     (statust (mapcar #'(lambda (x) (if (< x 2) 1 0)) deatht))
     (data (cosort lengtht arrivet statust xt yt zt)))
    (setf length (elt data 0))
    (setf arrive (elt data 1))
    (setf status (elt data 2))
    (setf x (elt data 3))
    (setf y (elt data 4))
    (setf z (elt data 5))
    nil))
;
; Plot for choosing cut point in covariate.
;
; Modified to add kernel density estimate.
; ENA - 06/24/93.
;
(defun choose-cut-plot (time stat covar)
  (let* ((data (km-plot time stat (kmest time stat)))
         (plot (plot-lines (first data) (second data)))
         (density (kernel-dens covar))
         (den-plot (plot-lines (first density) (second density)))
         (choose-slider (sequence-slider-dialog 
                         (rseq (min covar) (max covar) 50)
                         :action
                         #'(lambda (cut) 
                             (send plot :set-cut cut)
                             (send den-plot :set-cut cut)))))
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
             (send self :clear :draw nil)
             (send self :add-lines (first data-upper) (second data-upper))
             (send self :add-lines (first data-lower) (second data-lower))))
    (defmeth den-plot :set-cut (cut)
      (send self :clear :draw nil)
      (send self :add-lines (list (first density) (second density)))
      (send self :add-lines (list (list cut cut) (send self :range 1))))
    (send choose-slider :value 25)
    (send plot :set-cut (elt (rseq (min covar) (max covar) 50) 25))
    (send den-plot :set-cut (elt (rseq (min covar) (max covar) 50) 25))
    plot))
;
; generates a scatterplot matrix linked to
; an event chart.
;
; modified 6/24/93 to use 0/1 as status rather than nil/T - ENA.
;
(defun scat-event (arrive length status-in &rest covars)
  (let* 
    (
     (scat (scatterplot-matrix covars))
     (current-points (iseq (length arrive)))
     (event (send graph-proto :new 2))
     (max-arrive (max arrive))
     (max-length (max length))
     (arrive-range (get-nice-range 0 max-arrive 4))
     (length-range (get-nice-range 0 max-length 4))
     (status (= 1 status-in))
     )
    (send event :range 0 (select length-range 0) (select length-range 1))
    (send event :range 1 (select arrive-range 0) (select arrive-range 1))
    (send event :x-axis t t (select length-range 2))
    (send event :y-axis t t (select arrive-range 2))
    (send event :title "Event Chart")
    (send scat :point-state current-points 'hilited)
    (defmeth scat :unselect-all-points ()
      (setf current-points nil)
      (send event :clear)
      (call-next-method))
    (defmeth scat :adjust-points-in-rect (x1 y1 w h s)
      (let* ((p-i-r (send self :points-in-rect x1 y1 w h)))
        (setf current-points
              (case (send self :mouse-mode)
                (brushing p-i-r) 
                (selecting (union p-i-r current-points))))
        (if current-points
            (prog* 
             (
              i
              arrive-i
              length-i
              (arrive-sel (select arrive current-points))
              (length-sel (select length current-points))
              (status-sel (select status current-points))
              )
             (send event :clear :draw nil)
             (dotimes (i (length arrive-sel))
                      (def arrive-i (select arrive-sel i))
                      (def length-i (select length-sel i))
                      (send event :add-lines
                            (list
                             (list 0 length-i)
                             (list arrive-i arrive-i))
                            :draw nil
                            )
                      )
             (send event :add-points (list length-sel arrive-sel)
                   :draw nil)
             (if (member T status-sel)
                 (send event :point-symbol
                       (which status-sel)
                       'cross)
                 )
             (if (member NIL status-sel)
                 (send event :point-symbol
                       (which (map 'list #'not status-sel))
                       'diamond)
                 )
             (send event :redraw)
             )
            )
        (call-next-method x1 y1 w h s)))
    scat))
;
; generates a scatterplot matrix linked to
; a censored boxplot.
;
; ena - 6/1/92.
;
(require 'kmest)
(require 'quant)
(provide 'scat-box)
(defun scat-box (time status &rest covars)
  (let* 
    (
     (scat (scatterplot-matrix covars))
     (n (length (first covars)))
     (current-points (iseq n))
     (box (send graph-proto :new 2))
     (first-time (select time 0))
     (last-time (select time (- (length time) 1)))
     (time-range (get-nice-range first-time last-time 4))
     (ymin (select time-range 0))
     (ymax (select time-range 1))
     (yticks (select time-range 2))
     (xmin 0.667)
     (xmax 1.33)
     )
    (send box :range 0 0 2)
    (send box :range 1 ymin ymax)
    (send box :title "Censored Boxplot")
    (send box :y-axis t t yticks)
    (send box :add-slot 'xmin xmin)
    (send box :add-slot 'xmax xmax)
    (send box :add-slot 'mintime 0)
    (send box :add-slot 'maxtime 0)
    (send box :add-slot 'q25 0)
    (send box :add-slot 'q50 0)
    (send box :add-slot 'q75 0)
    (send box :add-slot 'minsurv-string "")
    (send box :add-slot 'maxsurv-string "")
    (send scat :point-state current-points 'hilited)
    (defmeth scat :unselect-all-points ()
      (setf current-points nil)
      (send box :clear)
      (setf q25 nil q50 nil q75 nil mintime nil maxtime nil)
      (send box :slot-value 'q25 nil)
      (send box :slot-value 'q50 nil)
      (send box :slot-value 'q75 nil)
      (send box :slot-value 'mintime nil)
      (send box :slot-value 'maxtime nil)
      (call-next-method)
      )
    (defmeth box :redraw-content ()
      (let* 
        (
         (temp (send self :resize))
         (xmin (send self :slot-value 'xmin))
         (xmax (send self :slot-value 'xmax))
         (mintime (send self :slot-value 'mintime))
         (maxtime (send self :slot-value 'maxtime))
         (q25 (send self :slot-value 'q25))
         (q50 (send self :slot-value 'q50))
         (q75 (send self :slot-value 'q75))
         (minsurv-string (send self :slot-value 'minsurv-string))
         (maxsurv-string (send self :slot-value 'maxsurv-string))
         )
        (cond
          (q25
           (send self :add-lines (list (list xmin xmax) (list q75 q75)))
           (send self :add-lines (list (list xmin xmax) (list q50 q50)))
           (send self :add-lines (list (list xmin xmax) (list q25 q25)))
           (send self :add-lines (list (list xmin xmin) (list q75 q25)))
           (send self :add-lines (list (list xmax xmax) (list q75 q25)))
           )
          (q50
           (send self :add-lines (list (list xmin xmax) (list q75 q75)))
            (send self :add-lines (list (list xmin xmax) (list q50 q50)))
           (send self :add-lines (list (list xmin xmin) (list q75 maxtime)))
           (send self :add-lines (list (list xmax xmax) (list q75 maxtime)))
           )
          (q75
           (send self :add-lines (list (list xmin xmax) (list q75 q75)))
           (send self :add-lines (list (list xmin xmin) (list q75 maxtime)))
           (send self :add-lines (list (list xmax xmax) (list q75 maxtime)))
           )
          (mintime
           (send self :add-lines (list (list xmin xmin) (list mintime 
                                                              maxtime)))
           (send self :add-lines (list (list xmax xmax) (list mintime 
                                                            maxtime)))
           )
          )
        (if mintime
            (prog*
             (
              (p1 (send self :real-to-canvas 1 mintime))
              (p2 (send self :real-to-canvas 1 maxtime))
              (p2x (first p2))
              (p2y (second p2))
              )
             (send self :draw-string maxsurv-string p2x p2y)))))
    (defmeth scat :adjust-points-in-rect (x1 y1 w h s)
      (let* 
        (
         (p-i-r (send self :points-in-rect x1 y1 w h))
         )
        (setf current-points
              (case 
                (send self :mouse-mode)
                (brushing p-i-r) 
                (selecting (union p-i-r current-points))))
        (if current-points
            (prog* 
             (
              (points (sort-data current-points))
              (time-sel (select time points))
              (status-sel (select status points))
              (new-km (kmest time-sel status-sel))
              (deaths (which (= 1 status-sel)))
              (death-times (select time-sel deaths))
              (mintime (select death-times 0))
              (maxtime (select death-times (- (length death-times) 1)))
              (death-surv (select new-km deaths))
              minsurv
              maxsurv
              (q25 (quant 0.25 time-sel new-km))
              (q50 (quant 0.50 time-sel new-km))
              (q75 (quant 0.75 time-sel new-km))
              minsurv-string
              maxsurv-string
              )
             (setf minsurv (if death-surv (select death-surv 0) nil))
             (setf maxsurv 
                   (if death-surv (select death-surv (- (length death-times) 1)) nil))
             (setf minsurv-string (format NIL "~S" minsurv))
             (setf maxsurv-string (format NIL "~S" maxsurv))
             (send box :slot-value 'mintime mintime)
             (send box :slot-value 'maxtime maxtime)
             (send box :slot-value 'q25 q25)
             (send box :slot-value 'q50 q50)
             (send box :slot-value 'q75 q75)
             (send box :slot-value 'minsurv-string minsurv-string)
             (send box :slot-value 'maxsurv-string maxsurv-string)
             (send box :redraw))
            (send self :unselect-all-points)
            )
        (call-next-method x1 y1 w h s)
        ))
    scat))
;
; Computes requested quantile of a survival curve.
;
; ENA - 6/1/92.
;
(defun quant (q times surv)
  (let*
    (
     (imax (min (which (>= q surv))))
     (imin (max (which (<= q surv))))
     (qout
      (if imax
          (let*
            (
             mintime
             minsurv
             slope
             interc
             (maxtime (select times imax))
             (maxsurv (select surv imax))
             )
            (setf mintime (if imin (select times imin) 0))
            (setf minsurv (if imin (select surv imin) 1))
            (setf slope 
                  (if (= maxtime mintime)
                      0
                      (/ (- maxsurv minsurv) (- maxtime mintime))
                      )
                  )
            (setf interc (- maxsurv (* slope maxtime)))
            (if (= slope 0) mintime (/ (- q interc) slope)))
          NIL
          )
      )
     )
    qout))
;
; Plot for checking accelerated failure model.
;
(require 'kmest)
(require 'km-plot)
(provide 'accel-fail-plot)
(defun accel-fail-plot (time1 stat1 time2 stat2)
  (let* ((data1 (km-plot time1 stat1 (kmest time1 stat1)))
         (data2 (km-plot time2 stat2 (kmest time2 stat2)))
         (max-time-1 (max time1))
         (max-time-2 (max time2))
         (slide-1-hi (if (>= max-time-1 max-time-2)
                         1 (/ max-time-2 max-time-1)))
         (slide-2-hi (if (< max-time-1 max-time-2)
                         1 (/ max-time-1 max-time-2)))
         (plot (plot-lines (first data1) (second data1)))
         (seq1 (sequence-slider-dialog (rseq 0 slide-1-hi 50)
                                       :action
                                       #'(lambda (m1) (send plot 
                                           :set-mult-1 m1))
                                       :title "data set 1"))
         (seq2 (sequence-slider-dialog (rseq 0 slide-2-hi 50)
                                       :action
                                       #'(lambda (m2) (send plot
                                           :set-mult-2 m2))
                                       :title "data set 2")))
    (send plot :add-lines (first data2) (second data2))
    (send plot :range 0 0 (max (append time1 time2)))
    (send plot :range 1 0 1)
    (send plot :add-slot 'mult1 slide-1-hi)
    (send plot :add-slot 'mult2 slide-2-hi)
    (defmeth plot :set-mult-1 (m1)
      (send self :change-plot (send self :slot-value 'mult1 m1)
            (send self :slot-value 'mult2)))
    (defmeth plot :set-mult-2 (m2)
      (send self :change-plot (send self :slot-value 'mult1)
            (send self :slot-value 'mult2 m2)))
    (defmeth plot :change-plot (m1 m2)
      (send self :clear :draw nil)
      (send self :add-lines (* m1 (first data1)) (second data1))
      (send self :add-lines (* m2 (first data2)) (second data2)))
    (send seq1 :value 50)
    (send seq2 :value 50)
    (send plot :change-plot slide-1-hi slide-2-hi)
    plot))
;
; plot to check proportional hazards model.
;
(require 'kmest)
(require 'km-plot)
(provide 'prop-haz-plot)
(defun prop-haz-plot (time1 stat1 time2 stat2)
  (let* ((data1 (km-plot time1 stat1 (kmest time1 stat1)))
         (data2 (km-plot time2 stat2 (kmest time2 stat2)))
         (plot (plot-lines (first data1) (second data1)))
         (seq1 (sequence-slider-dialog (rseq 0 1 50)
                                       :action
                                       #'(lambda (p1) (send plot 
                                           :set-power-1 p1))
                                       :title "data set 1"))
         (seq2 (sequence-slider-dialog (rseq 0 1 50)
                                       :action
                                       #'(lambda (p2) (send plot
                                           :set-power-2 p2))
                                       :title "data set 2")))
    (send plot :add-lines (first data2) (second data2))
    (send plot :range 0 0 (max (append time1 time2)))
    (send plot :range 1 0 1)
    (send plot :add-slot 'pow1 1)
    (send plot :add-slot 'pow2 1)
    (defmeth plot :set-power-1 (p1)
      (send self :change-plot (send self :slot-value 'pow1 p1)
            (send self :slot-value 'pow2)))
    (defmeth plot :set-power-2 (p2)
      (send self :change-plot (send self :slot-value 'pow1)
            (send self :slot-value 'pow2 p2)))
    (defmeth plot :change-plot (p1 p2)
      (send self :clear :draw nil)
      (send self :add-lines (first data1) (^ (second data1) p1))
      (send self :add-lines (first data2) (^ (second data2) p2)))
    (send seq1 :value 50)
    (send seq2 :value 50)
    plot))
;
; generate data from exponential
; distribution with censoring. censoring
; times are generated as uniform random
; numbers between 0 and time of death.
;
(provide 'gen-expo-data)
(require 'exp-rand)
(require 'cosort)
(defun gen-expo-data (num1 num2 prob-cen l1 l2)
  (setf time1 (exp-rand (repeat l1 num1)))
  (setf time2 (exp-rand (repeat l2 num2)))
  (setf stat1 (binomial-rand num1 1 (- 1 prob-cen)))
  (setf stat2 (binomial-rand num2 1 (- 1 prob-cen)))
  (mapcar #'(lambda (i) (if (= (elt stat1 i) 0)
                            (setf (select time1 i)
                                  (first (* (elt time1 i) 
                                            (uniform-rand 1))))))
          (iseq (length time1)))
  (mapcar #'(lambda (i) (if (= (elt stat2 i) 0)
                            (setf (select time2 i)
                                  (first (* (elt time2 i)
                                            (uniform-rand 1))))))
          (iseq (length time2)))
  (setf data1 (cosort time1 stat1))
  (setf time1 (elt data1 0))
  (setf stat1 (elt data1 1))
  (setf data2 (cosort time2 stat2))
  (setf time2 (elt data2 0))
  (setf stat2 (elt data2 1))
  nil)
;
; Generate a specific sample for the
; proportional hazards plot, i.e. call
; gen-expo-data with some specific parameters.
;
(require 'gen-expo-data)
(provide 'gen-expo-example)
(defun gen-expo-example ()
  (gen-expo-data 100 50 .1 1 10)
  (setf data1 (cosort time1 stat1))
  (setf time1 (elt data1 0))
  (setf stat1 (elt data1 1))
  nil)
