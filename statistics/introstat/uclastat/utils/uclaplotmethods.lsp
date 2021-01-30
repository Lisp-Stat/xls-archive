;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3 Plot Objects and Methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth histogram-proto :plot (data)
  (send self :allocate)
  (send self :add-points data)
  (send self :adjust-to-data)
  (send self :add-menu-items)
  (send self :linked t)
)

(defmeth histogram-proto :add-menu-items ()
(let (
     (d2 (send dash-item-proto :new))
     (ai (send menu-item-proto :new "Save"
              :action #'(lambda () (send self :save))))
     )
(mapcar #'(lambda (x) (send (send self :menu) :append-items x)) 
      (list d2 ai) )
))

(defmeth histogram-proto :remove-menu-items ()
(let (
     (m (send self :menu))
     )
(send m :delete-items 
      (send m :find-item "Save"))
(send m :delete-items
      (first (reverse (send m :items)))) 
))

(if (find 'macintosh *features*)
(defmeth display-window-proto :plot (data)
(send self :paste-string 
      (stem-and-leaf-plot data (eval-string-input (send self :stem))))
))

(defmeth scatterplot-proto :plot (data &optional (labels nil set))
(send self :allocate)
(cond 
  ((string= (send self :slot-value 'plot-type) "box-plot")
   (let (
         (range (get-nice-range (min data) (max data) 4))
         )
     (send self :range 1 (nth 0 range) (nth 1 range))
     (send self :y-axis t nil (nth 2 range))
     (send self :range 0 0 2)
     (send self :add-boxplot data)
     (send self :add-menu-items)))
  ((string= (send self :slot-value 'plot-type) "spike-plot")
   (dolist (i data)
     (send self :add-lines (repeat i 2) '(0 1)))
   (send self :variable-label
         '(0 1) '("Value" "Presence"))
   (send self :x-axis t t 4)
   (send self :y-axis t t 2)
   (send self :adjust-to-data)
   (send self :add-menu-items))
  ((string= (send self :slot-value 'plot-type) "kernel-plot")
   (send self :add-lines (kernel-dens data))
   (send self :variable-label 
         '(0 1) '("Value" "Density"))
   (send self :x-axis t t 4)
   (send self :y-axis t t 4)
   (send self :adjust-to-data)
   (send self :add-menu-items))
  ((string= (send self :slot-value 'plot-type) "quantile-plot")
   (let (
         (f (intern (string-upcase (concatenate 'string compar "-quant"))))
         (x (funcall f (/ (1+ (rank data)) (1+ (length data)))))
         (s (concatenate 'string compar " Quantiles"))
         )
     (send self :add-points x data)
     (mapcar #'(lambda (i) (send self :point-label i (elt labels i)))
             (iseq (length data)))
     (send self :add-menu-items)
     (send self :variable-label 
           '(0 1) '("Normal Quantiles" "Observed Quantiles"))
     (send self :x-axis t t 5)
     (send self :y-axis t t 5)
     (send self :linked t)
     (send self :adjust-to-data)))
  ((string= (send self :slot-value 'plot-type) "probability-plot")
   (let (
         (x (/ (1+ (rank data)) (1+ (length data))))
         (y (normal-cdf data))
         )
     (send self :add-points x y)
     (send self :add-menu-items)
     (send self :variable-label 
           '(0 1) '("Normal CDF" "Observed CDF"))
     (send self :x-axis t t 5)
     (send self :y-axis t t 5)
     (send self :linked t)
     (send self :adjust-to-data)))
  ((string= (send self :slot-value 'plot-type) "edf-plot")
   (let (
         (x (/ (1+ (rank data)) (1+ (length data))))
         )
     (send self :add-points data x)
     (send self :add-menu-items)
     (send self :variable-label 
           '(0 1) '("Value" "CDF"))
     (send self :x-axis t t 5)
     (send self :y-axis t t 5)
     (send self :linked t)
     (send self :adjust-to-data)))
  ((string= (send self :slot-value 'plot-type) "time-plot")
   (let (
         (x (iseq (length data)))
         )
     (send self :add-points x data)
     (send self :add-menu-items)
     (send self :variable-label 
           '(0 1) '("Time" "Value"))
     (send self :x-axis t t 5)
     (send self :y-axis t t 5)
     (send self :linked t)
     (send self :adjust-to-data)))
))

(defmeth scatterplot-proto :add-menu-items ()
(cond 
 ((string= (send self :slot-value 'plot-type) "box-plot")
  (let (
        (d2 (send dash-item-proto :new))
        (ai (send menu-item-proto :new "Save"
              :action #'(lambda () (send self :save))))
        )
    (mapcar #'(lambda (x) (send (send self :menu) :append-items x)) 
      (list d2 ai))))
 ((string= (send self :slot-value 'plot-type) "spike-plot")
  (let (
        (d2 (send dash-item-proto :new))
        (ai (send menu-item-proto :new "Save"
                  :action #'(lambda () (send self :save))))
        )
    (mapcar #'(lambda (x) (send (send self :menu) :append-items x)) 
            (list d2 ai))))
 ((string= (send self :slot-value 'plot-type) "kernel-plot")
 (let (
       (d2 (send dash-item-proto :new))
       (ai (send menu-item-proto :new "Save"
                 :action #'(lambda () (send self :save))))
       )
   (mapcar #'(lambda (x) (send (send self :menu) :append-items x)) 
           (list d2 ai))))
((string= (send self :slot-value 'plot-type) "time-plot")
 (let (
       (d1 (send dash-item-proto :new))
       (ci (send menu-item-proto :new "Connect Points"
              :action #'(lambda () (send self :connect-points))))
       (li (send menu-item-proto :new "Best Line"
              :action #'(lambda () (send self :best-line))))
       (ii (send menu-item-proto :new "Identity Line"
              :action #'(lambda () (send self :identity-line))))
       (oi (send menu-item-proto :new "Lowess Smoother"
              :action #'(lambda () (send self :lowess))))
       (si (send menu-item-proto :new "Spline Interpolant"
              :action #'(lambda () (send self :spline))))
       (ki (send menu-item-proto :new "Kernel Smoother"
              :action #'(lambda () (send self :kernel-smooth))))
       (ri (send menu-item-proto :new "Clear Lines"
              :action #'(lambda () (send self :clear-lines))))
       (d2 (send dash-item-proto :new))
       (ai (send menu-item-proto :new "Save"
                 :action #'(lambda () (send self :save))))
       )
   (mapcar #'(lambda (x) (send (send self :menu) :append-items x)) 
           (list d1 ci li ii oi si ki ri d2 ai))))
((string= (send self :slot-value 'plot-type) "quantile-plot")
 (let (
       (m (send self :menu))
       (l (list "Connect Points" "Best Line" "Identity Line"
                "Lowess Smoother" "Spline Interpolant"
                "Kernel Smoother" "Clear Lines" "Save"))
       )
   (mapcar #'(lambda (x) (send m :delete-items 
                               (send m :find-item x))) l)
   (send m :delete-items
         (first (reverse (send m :items)))) 
   (send m :delete-items
         (first (reverse (send m :items))))))
((string= (send self :slot-value 'plot-type) "probability-plot")
 (let (
       (d1 (send dash-item-proto :new))
       (ci (send menu-item-proto :new "Connect Points"
                 :action #'(lambda () (send self :connect-points))))
       (li (send menu-item-proto :new "Best Line"
              :action #'(lambda () (send self :best-line))))
       (ii (send menu-item-proto :new "Identity Line"
                 :action #'(lambda () (send self :identity-line))))
       (oi (send menu-item-proto :new "Lowess Smoother"
                 :action #'(lambda () (send self :lowess))))
       (si (send menu-item-proto :new "Spline Interpolant"
                 :action #'(lambda () (send self :spline))))
       (ki (send menu-item-proto :new "Kernel Smoother"
                 :action #'(lambda () (send self :kernel-smooth))))
       (ri (send menu-item-proto :new "Clear Lines"
                 :action #'(lambda () (send self :clear-lines))))
       (d2 (send dash-item-proto :new))
       (ai (send menu-item-proto :new "Save"
                 :action #'(lambda () (send self :save))))
       )
   (mapcar #'(lambda (x) (send (send self :menu) :append-items x)) 
           (list d1 ci li ii oi si ki ri d2 ai))))
((string= (send self :slot-value 'plot-type) "edf-plot")
 (let (
       (d1 (send dash-item-proto :new))
       (ci (send menu-item-proto :new "Connect Points"
                 :action #'(lambda () (send self :connect-points))))
       (oi (send menu-item-proto :new "Lowess Smoother"
                 :action #'(lambda () (send self :lowess))))
       (si (send menu-item-proto :new "Spline Interpolant"
                 :action #'(lambda () (send self :spline))))
       (ki (send menu-item-proto :new "Kernel Smoother"
                 :action #'(lambda () (send self :kernel-smooth))))
       (ri (send menu-item-proto :new "Clear Lines"
                 :action #'(lambda () (send self :clear-lines))))
       (d2 (send dash-item-proto :new))
       (ai (send menu-item-proto :new "Save"
                 :action #'(lambda () (send self :save))))
       )
   (mapcar #'(lambda (x) (send (send self :menu) :append-items x)) 
           (list d1 ci li ii oi si ki ri d2 ai))))
))

(defmeth scatterplot-proto :remove-menu-items ()
(cond
 ((string= (send self :slot-value 'plot-type) "box-plot")
  (let (
        (m (send self :menu))
        )
    (send m :delete-items 
          (send m :find-item "Save"))
    (send m :delete-items
          (first (reverse (send m :items))))))
 ((string= (send self :slot-value 'plot-type) "spike-plot")
  (let (
        (m (send self :menu))
        )
    (send m :delete-items 
          (send m :find-item "Save"))
    (send m :delete-items
          (first (reverse (send m :items))))))
 ((string= (send self :slot-value 'plot-type) "kernel-plot")
  (let (
        (m (send self :menu))
        )
    (send m :delete-items 
          (send m :find-item "Save"))
    (send m :delete-items
          (first (reverse (send m :items)))))) 
 ((string= (send self :slot-value 'plot-type) "time-plot")
  (let (
        (m (send self :menu))
        (l (list "Connect Points" "Best Line" "Identity Line"
                 "Lowess Smoother" "Spline Interpolant"
                 "Kernel Smoother" "Clear Lines" "Save"))
        )
    (mapcar #'(lambda (x) (send m :delete-items 
                                (send m :find-item x))) l)
    (send m :delete-items
          (first (reverse (send m :items)))) 
    (send m :delete-items
          (first (reverse (send m :items))))))
 ((string= (send self :slot-value 'plot-type) "quantile-plot")
  (let (
        (m (send self :menu))
        (l (list "Connect Points" "Best Line" "Identity Line"
                 "Lowess Smoother" "Spline Interpolant"
                 "Kernel Smoother" "Clear Lines" "Save"))
        )
    (mapcar #'(lambda (x) (send m :delete-items 
                                (send m :find-item x))) l)
    (send m :delete-items
          (first (reverse (send m :items)))) 
    (send m :delete-items
          (first (reverse (send m :items))))))
 ((string= (send self :slot-value 'plot-type) "probability-plot")
  (let (
        (m (send self :menu))
        (l (list "Connect Points" "Best Line" "Identity Line"
                 "Lowess Smoother" "Spline Interpolant"
                 "Kernel Smoother" "Clear Lines" "Save"))
        )
    (mapcar #'(lambda (x) (send m :delete-items 
                                (send m :find-item x))) l)
    (send m :delete-items
          (first (reverse (send m :items)))) 
    (send m :delete-items
          (first (reverse (send m :items)))))) 
 ((string= (send self :slot-value 'plot-type) "edf-plot")
  (let (
        (m (send self :menu))
        (l (list "Connect Points"
                 "Lowess Smoother" "Spline Interpolant"
                 "Kernel Smoother" "Clear Lines" "Save"))
        )
    (mapcar #'(lambda (x) (send m :delete-items 
                                (send m :find-item x))) l)
    (send m :delete-items
          (first (reverse (send m :items)))) 
    (send m :delete-items
          (first (reverse (send m :items))))))
))

(defmeth scatterplot-proto :close ()
(call-next-method)
(send self :remove-menu-items))

(defmeth scatterplot-proto :connect-points ()
(let* (
     (n (send self :num-points))
     (x (send self :point-coordinate 0 (iseq n)))
     (y (send self :point-coordinate 1 (iseq n)))
     (r (order x))
     )
(send self :add-lines (select x r) (select y r))
))

(defmeth scatterplot-proto :best-line ()
(let* (
     (n (send self :num-points))
     (x (send self :point-coordinate 0 (iseq n)))
     (y (send self :point-coordinate 1 (iseq n)))
     (mx (mean x))
     (my (mean y))
     (sxx (sum (^ (- x mx) 2)))
     (syy (sum (^ (- y my) 2)))
     (sxy (sum (* (- x mx) (- y my))))
     (b (/ sxy sxx))
     (a (- my (* b mx)))
     )
(send self :abline a b)
))

(defmeth scatterplot-proto :identity-line ()
(send self :abline 0 1)
)

(defmeth scatterplot-proto :lowess ()
(let* (
     (n (send self :num-points))
     (x (send self :point-coordinate 0 (iseq n)))
     (y (send self :point-coordinate 1 (iseq n)))
     )
(send self :add-lines (lowess x y))
))

(defmeth scatterplot-proto :spline ()
(let* (     
     (n (send self :num-points))
     (x (send self :point-coordinate 0 (iseq n)))
     (y (send self :point-coordinate 1 (iseq n)))
     (r (order x))
     )
(send self :add-lines 
      (spline (select x (order x)) (select y (order y))))
))

(defmeth scatterplot-proto :kernel-smooth ()
(let* (
     (n (send self :num-points))
     (x (send self :point-coordinate 0 (iseq n)))
     (y (send self :point-coordinate 1 (iseq n)))
     )
(send self :add-lines (kernel-smooth x y))
))

(defmeth menu-proto :find-item (string)
(let* (
     (il (send self :items))
     (ll (mapcar #'(lambda(x) (send x :title)) il))
     )
(select il (position string ll :test #'string-equal))
))












